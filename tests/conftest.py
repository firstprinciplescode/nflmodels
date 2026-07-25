"""Shared pytest fixtures for Lambda cleaner tests.

Cleaner filenames contain hyphens, so they are loaded via importlib rather than
normal imports. boto3.client() at module import time does not require AWS
credentials (only API calls do), so imports are safe in any environment.
"""
import importlib.util
import sys
from pathlib import Path

CLEANERS_DIR = Path(__file__).resolve().parents[1] / "lambdas" / "cleaners"


def load_cleaner(filename: str):
    """Import a cleaner module by filename (e.g. 'rushing-summary-cleaner.py')."""
    path = CLEANERS_DIR / filename
    mod_name = filename.replace("-", "_").removesuffix(".py")
    spec = importlib.util.spec_from_file_location(mod_name, path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[mod_name] = module
    spec.loader.exec_module(module)
    return module
