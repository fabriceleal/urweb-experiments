CREATE SEQUENCE uw_Database_postSeq;
 
 CREATE SEQUENCE uw_Database_positionSeq;
  
  CREATE SEQUENCE uw_Database_commentSeq;
   
   CREATE SEQUENCE uw_Database_inviteSeq;
    
    CREATE TABLE uw_Database_position(uw_id int8 NOT NULL, 
                                       uw_postid int8 NOT NULL, 
                                       uw_fen text NOT NULL, uw_move text, 
                                       uw_movealg text, 
                                       uw_previouspositionid int8,
     PRIMARY KEY (uw_id),
      CONSTRAINT uw_Database_position_CPreviousPositionId
       FOREIGN KEY (uw_previousPositionId) REFERENCES uw_Database_position (uw_id) ON DELETE CASCADE
     );
     
     CREATE TABLE uw_Database_post(uw_id int8 NOT NULL, uw_nam text NOT NULL, 
                                    uw_rootpositionid int8 NOT NULL, 
                                    uw_userid int8 NOT NULL, 
                                    uw_currentpositionid int8 NOT NULL, 
                                    uw_parentpostid int8, 
                                    uw_room int8 NOT NULL, 
                                    uw_posttype int8 NOT NULL,
      PRIMARY KEY (uw_id)
       
      );
      
      CREATE TABLE uw_Database_comment(uw_id int8 NOT NULL, 
                                        uw_positionid int8 NOT NULL, 
                                        uw_content text NOT NULL, 
                                        uw_userid int8 NOT NULL, 
                                        uw_sent timestamp NOT NULL,
       PRIMARY KEY (uw_id)
        
       );
       
       CREATE SEQUENCE uw_Database_userSeq;
        
        CREATE TABLE uw_Database_user(uw_id int8 NOT NULL, 
                                       uw_nam text NOT NULL, 
                                       uw_pass text NOT NULL, 
                                       uw_salt text NOT NULL,
         PRIMARY KEY (uw_id),
          CONSTRAINT uw_Database_user_Nam UNIQUE (uw_nam)
         );
         
         CREATE TABLE uw_Database_invite(uw_id int8 NOT NULL, 
                                          uw_userid int8 NOT NULL, 
                                          uw_invitedid int8, 
                                          uw_code text NOT NULL, 
                                          uw_email text NOT NULL, 
                                          uw_sent timestamp NOT NULL, 
                                          uw_status int8 NOT NULL,
          PRIMARY KEY (uw_id),
           CONSTRAINT uw_Database_invite_Code UNIQUE (uw_code),
                                                               
            CONSTRAINT uw_Database_invite_Email UNIQUE (uw_email)
          );
          
          CREATE TABLE uw_Database_rootAdmin(uw_id int8 NOT NULL,
           PRIMARY KEY (uw_id),
            CONSTRAINT uw_Database_rootAdmin_Id
             FOREIGN KEY (uw_id) REFERENCES uw_Database_user (uw_id)
           );
           
           CREATE SEQUENCE uw_Helloworld_ChessRoom_s;
            
            CREATE TABLE uw_Helloworld_ChessRoom_t(uw_id int8 NOT NULL, 
                                                    uw_client int4 NOT NULL, 
                                                    uw_channel int8 NOT NULL,
             PRIMARY KEY (uw_client, uw_id)
              
             );
             
             