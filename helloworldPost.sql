
CREATE INDEX uw_database_position_previousPositionId ON uw_database_position (uw_PreviousPositionId);

CREATE INDEX uw_database_position_postId ON uw_database_position (uw_PostId);

CREATE INDEX uw_sharedboard_chessroom_t_Channel ON uw_sharedboard_chessroom_t (uw_Channel);
