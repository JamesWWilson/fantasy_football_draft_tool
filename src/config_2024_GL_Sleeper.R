# config.R

config <-list(
  
  # data sources 
  fantasypros_source = "../data/2024/FantasyPros_2024_Draft_ALL_Rankings_HalfPPR.csv",
  manual_PlayerNotes = "../data/2024/2024_PlayerNotes.xlsx",
  
  
  scoring_summary = "Half-PPR",
  team_count = 14,
  league_name = "GentlemensLeague",
  positions = c("QB","RB","WR","TE","K","DST"),
  season_choice = 2024,
  
  # sleeper_league_half_ppr_points
  scoring_rules = list(
      pass = list(
        pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.04, pass_tds = 4,
        pass_int = -2, pass_40_yds = 2,  pass_300_yds = 2, pass_350_yds = 2,
        pass_400_yds = 2 #pass_int = -2
      ),
      rush = list(
        all_pos = TRUE,
        rush_yds = 0.1,  rush_att = 0, rush_40_yds = 1, rush_tds = 6,
        rush_100_yds = 2, rush_150_yds = 2, rush_200_yds = 2),
      rec = list(
        all_pos = TRUE,
        rec = 0.5, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 1, rec_100_yds = 2,
        rec_150_yds = 2, rec_200_yds = 2
      ),
      misc = list(
        all_pos = TRUE,
        fumbles_lost = -2, fumbles_total = 0,
        sacks = 0, two_pts = 2
      ),
      kick = list(
        xp = 1.0, fg_0019 = 3.0,  fg_2029 = 3.0, fg_3039 = 3.0, fg_4049 = 4.0,
        fg_50 = 5.0,  fg_miss = -2.5
      ),
      ret = list(
        all_pos = TRUE,
        return_tds = 6, return_yds = 0
      ),
      idp = list(
        all_pos = TRUE,
        idp_solo = 1, idp_asst = 0.5, idp_sack = 2, idp_int = 3,  idp_fum_force = 3,
        idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2
      ),
      dst = list(
        dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
        dst_blk = 3, dst_ret_yds = 0, dst_pts_allowed = 0
      ),
      pts_bracket = list(
        list(threshold = 0, points = 10),
        list(threshold = 6, points = 7),
        list(threshold = 13, points = 4),
        list(threshold = 20, points = 1),
        list(threshold = 34, points = -1),
        list(threshold = 99, points = -4)
      )
))