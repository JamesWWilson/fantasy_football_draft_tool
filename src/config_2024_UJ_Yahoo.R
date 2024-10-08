# config.R

config <-list(
  
  scoring_summary = "PPR", #options: Standard / PPR / Half-PPR
  team_count = 10,
  league_name = "UncleJim",
  positions = c("QB","RB","WR","TE","K","DST"),
  season_choice = 2024,
  
  scoring_rules = list(
    yahoo_jim_standard_points = list(
      pass = list(
        pass_att = 0, pass_comp = 0, pass_inc = 0, pass_yds = 0.25, pass_tds = 6,
        pass_int = -2, pass_40_yds = 0,  pass_300_yds = 0, pass_350_yds = 0,
        pass_400_yds = 0
      ),
      rush = list(
        all_pos = TRUE,
        rush_yds = 0.1,  rush_att = 0, rush_40_yds = 0, rush_tds = 6,
        rush_100_yds = 0, rush_150_yds = 0, rush_200_yds = 0),
      rec = list(
        all_pos = TRUE,
        rec = 1, rec_yds = 0.1, rec_tds = 6, rec_40_yds = 0, rec_100_yds = 0,
        rec_150_yds = 0, rec_200_yds = 0
      ),
      misc = list(
        all_pos = TRUE,
        fumbles_lost = -2, fumbles_total = 0,
        sacks = 0, two_pts = 2
      ),
      kick = list(
        xp = 1.0, fg_0019 = 1.0,  fg_2029 = 2.0, fg_3039 = 3.0, fg_4049 = 4.0,
        fg_50 = 5.0,  fg_miss = 0.0
      ),
      ret = list(
        all_pos = TRUE,
        return_tds = 6, return_yds = 0.33
      ),
      idp = list(
        all_pos = TRUE,
        idp_solo = 1, idp_asst = 0.5, idp_sack = 1, idp_int = 2,  idp_fum_force = 3,
        idp_fum_rec = 2,  idp_pd = 1, idp_td = 6,  idp_safety = 2
      ),
      dst = list(
        dst_fum_rec = 2,  dst_int = 2, dst_safety = 2, dst_sacks = 1, dst_td = 6,
        dst_blk = 1.5, dst_ret_yds = 0, dst_pts_allowed = 0
      ),
      pts_bracket = list(
        list(threshold = 0, points = 10),
        list(threshold = 6, points = 7),
        list(threshold = 13, points = 4),
        list(threshold = 20, points = 1),
        list(threshold = 27, points = 0),
        list(threshold = 34, points = -1),
        list(threshold = 99, points = -4)
      )
    )
  ))