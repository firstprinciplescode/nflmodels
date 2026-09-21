source("pff_stats/evaluation/waterfall_bridge.R")
source("pff_stats/evaluation/impact_bridge.R")

# ---------------- 1. KC SAFETIES: Conner / Cook  ->  Hicks / Gilman ----------------
cov_card("Chamarri Conner"); 
cov_card("Bryan Cook")          # who's leaving, season by season

cov_card("Jaden Hicks");     
cov_card("Alohi Gilman")        # who's coming in

rd_card("Bryan Cook");       
rd_card("Alohi Gilman")         # safeties in run defense

wf_pvp(c("run_defense"),
       a = c("DJ Reader", "Brian Branch"),
       b = c("Alim McNeil", "Chuck Clark"))                 # effect on passing AND rushing allowed

# ---------------- 2. KC CORNERS: Watson / McDuffie  ->  rookie / Sneed ----------------
cov_card("Jaylen Watson"); 
cov_card("Trent McDuffie")
cov_card("Alex Anzalone")                                   # shows his thin seasons too
wf_pvp("run_block",
       a = c("David Edwards"),
       b = c("Alec Anderson"))                    # rookie = your entry-year CB prior
wf_pvp("secondary", a = "Jaylen Watson", b = 0.40)           # or your own number for the rookie

# ---------------- 3. KC TACKLES: Simmons / Taylor  ->  Moore / rookie ----------------
ot_card("Josh Simmons"); 
ot_card("Jawaan Taylor"); 
ot_card("Jaylon Moore")
wf_pvp(c("secondary"),
       a = c("Andrew Wingard", "Devin Lloyd", "Greg Newsome II"),
       b = c("ROOKIE", "VET BACKUP", "ROOKIE"))                      # KC's own passing AND rushing
wf_pvp(c("pass_block", "run_block"), a = c("Patrick Mekari"), b = c("ROOKIE"))
# the Simmons injury games only: Moore at LT for Simmons
wf_pvp(c("pass_block", "run_block"), a = "Josh Simmons", b = "Jaylon Moore")

# ---------------- 4. WADDLE on DENVER (Nix 2025 offense) ----------------
wr_card("DJ Moore")
wf_pvp("receiving", a = "AVERAGE",       b = "Jaylen Waddle")   # Waddle vs a league-average receiver in his role
wf_pvp("pass_rush", a = "Troy Franklin", b = "Jaylen Waddle")   # Waddle taking Franklin's role
waterfall("DEN", "pass_off")                                    # Denver 2025 (Nix) -> 2026 passing, whole corps + OL

# ---------------- 5. WHOLE TEAMS AT A GLANCE ----------------
waterfall("DEN", "rush_off")     # corners + safeties + pass rush -> passing allowed
waterfall("JAX", "rush_off")     # run defense incl. safeties -> rushing allowed
waterfall("DEN", "pass_def")     # OL (all 5 slots) + corps -> KC passing
waterfall("JAX", "rush_def")     # OL run blocking + backs -> KC rushing
