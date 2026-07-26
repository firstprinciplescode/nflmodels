"""Unit tests for Lambda cleaner functions.

The handlers are exercised exactly as written; S3 is replaced with an in-memory
fake so no AWS access occurs and no real data can be touched.
"""
from io import BytesIO
from pathlib import Path

import pandas as pd
import pytest

from conftest import CLEANERS_DIR, load_cleaner


class FakeS3:
    """Minimal in-memory stand-in for the boto3 S3 client used by the cleaners."""

    class exceptions:
        class NoSuchKey(Exception):
            pass

    def __init__(self, objects=None):
        self.objects = dict(objects or {})
        self.put_calls = []

    def get_object(self, Bucket, Key):
        if Key not in self.objects:
            raise self.exceptions.NoSuchKey(Key)
        return {"Body": BytesIO(self.objects[Key])}

    def put_object(self, Bucket, Key, Body, **kwargs):
        self.objects[Key] = Body
        self.put_calls.append(Key)
        return {}


def parquet_bytes(df: pd.DataFrame) -> bytes:
    buf = BytesIO()
    df.to_parquet(buf, index=False, engine="pyarrow")
    return buf.getvalue()


@pytest.fixture()
def rushing():
    return load_cleaner("rushing-summary-cleaner.py")


def test_int_columns_coerced_to_float(rushing):
    """int64 columns must be rewritten as float64 (Athena schema consistency)."""
    df = pd.DataFrame({"player_id": [1, 2], "yards": [50, 75], "name": ["a", "b"]})
    key = "data/rushing_summary/season=2024/data.parquet"
    fake = FakeS3({key: parquet_bytes(df)})
    rushing.s3_client = fake

    result = rushing.lambda_handler({"seasons": [2024]}, None)

    assert result["statusCode"] == 200
    assert result["body"][0] == {"season": 2024, "records": 2, "status": "success"}
    written = pd.read_parquet(BytesIO(fake.objects[key]))
    assert written["player_id"].dtype == "float64"
    assert written["yards"].dtype == "float64"
    assert written["name"].tolist() == ["a", "b"]  # non-int columns untouched
    assert written["yards"].tolist() == [50.0, 75.0]  # values preserved


def test_seasons_all_expands_to_full_range(rushing):
    """'all' must fan out to seasons 2015-2027 (13 attempts)."""
    fake = FakeS3()  # empty bucket -> every season reports no_data
    rushing.s3_client = fake

    result = rushing.lambda_handler({"seasons": "all"}, None)

    assert [r["season"] for r in result["body"]] == list(range(2015, 2028))
    assert all(r["status"] == "no_data" for r in result["body"])


def test_missing_key_reports_no_data_and_writes_nothing(rushing):
    fake = FakeS3()
    rushing.s3_client = fake

    result = rushing.lambda_handler({"seasons": [2019]}, None)

    assert result["body"] == [{"season": 2019, "status": "no_data"}]
    assert fake.put_calls == []


def test_unexpected_error_is_captured_not_raised(rushing):
    """A corrupt object must yield status=error for that season, not an exception."""
    key = "data/rushing_summary/season=2024/data.parquet"
    fake = FakeS3({key: b"not a parquet file"})
    rushing.s3_client = fake

    result = rushing.lambda_handler({"seasons": [2024]}, None)

    assert result["body"][0]["season"] == 2024
    assert result["body"][0]["status"] == "error"
    assert "error" in result["body"][0]


def test_default_event_targets_2025(rushing):
    fake = FakeS3()
    rushing.s3_client = fake
    result = rushing.lambda_handler({}, None)
    assert [r["season"] for r in result["body"]] == [2025]


@pytest.mark.parametrize(
    "filename",
    sorted(p.name for p in CLEANERS_DIR.glob("*.py")),
)
def test_every_cleaner_imports_and_exposes_handler(filename):
    """Smoke test: each cleaner module loads without AWS access and defines lambda_handler."""
    module = load_cleaner(filename)
    assert callable(getattr(module, "lambda_handler", None))
