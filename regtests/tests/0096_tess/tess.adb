------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                      Copyright (C) 2005-2012, AdaCore                    --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Text_Io; use Ada.Text_Io;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Templates_Parser; use Templates_Parser;

procedure Tess is

   function "+"
     (Str : String)
      return Unbounded_String
      renames To_Unbounded_String;

   type PS is array (Positive range <>, Positive range <>) of Unbounded_String;

   People_Set : PS
     := ((+"LBOZJAQjltçhox", +"187", +"13", +"Yzfihjlojz"),
         (+"LGFOAFhzc", +"188", +"13", +"Yzfihjlojz"),
         (+"LSYHBKALao", +"127", +"10", +"O27"),
         (+"LTPLTXHTAEjkth", +"77", +"7", +"O24"),
         (+"LJBLKPAGzlt-Iroaoiiz", +"56", +"6", +"O23"),
         (+"LKDALOJAPhfotowkz", +"39", +"5", +"O22"),
         (+"ELPHALYHALtbzal", +"40", +"5", +"O22"),
         (+"ELTBOAADhjottz", +"128", +"10", +"O27"),
         (+"ELJEJLKAYAIlyjods", +"129", +"3", +"O20"),
         (+"ELJEJLKAYAIlyjods", +"129", +"10", +"O27"),
         (+"ELJJLKAYAFlvofz", +"174", +"13", +"Yzfihjlojz"),
         (+"EZPKAIroaoiiz", +"41", +"5", +"O22"),
         (+"EZAAALFUAGzlt-Dalkpz", +"24", +"4", +"O21A-AODX"),
         (+"EZTULROLADrljlq", +"190", +"13", +"Yzfihjlojz"),
         (+"EZJBZAAkphmod", +"163", +"12", +"O29"),
         (+"EZJYRHKAGzlt-Umzx", +"109", +"3", +"O20"),
         (+"EZJYRHKAGzlt-Umzx", +"109", +"9", +"O26"),
         (+"EZJYJLTPAGkaozt", +"228", +"13", +"Yzfihjlojz"),
         (+"EOPLTAGzlt-Umzx", +"57", +"6", +"O23"),
         (+"EOZAXSOATodhalx", +"94", +"8", +"O25A-AYLOD"),
         (+"EOHKVAIroaoiiz", +"130", +"10", +"O27"),
         (+"EHDDHT-BOEHPARztju", +"95", +"8", +"O25A-AYLOD"),
         (+"EHPZYAIlyjods", +"42", +"5", +"O22"),
         (+"EHOYZLKAHaomozj", +"58", +"6", +"O23"),
         (+"EHFILXAQjltçhoxz", +"59", +"6", +"O23"),
         (+"EHKDRZAYrozjju", +"191", +"13", +"Yzfihjlojz"),
         (+"EHKPZMOAAZAHaomozj", +"43", +"5", +"O22"),
         (+"EHKJRLTOARlaofl", +"192", +"13", +"Yzfihjlojz"),
         (+"EJLGZKAAIroaoiiz", +"25", +"4", +"O21A-AODX"),
         (+"EJZYLKAYALjfzaaz", +"148", +"11", +"O28"),
         (+"DLJZFHAOADrjoxyolt", +"110", +"9", +"O26"),
         (+"DZTXOZJADlyrzjotz", +"3", +"3", +"O20"),
         (+"DRLELKAYADrjoxyotz", +"193", +"13", +"Yzfihjlojz"),
         (+"DRZMLAAOZJADlyrzjotz", +"149", +"11", +"O28"),
         (+"DAZFZTYAEjkth", +"44", +"5", +"O22"),
         (+"DHAAZYYZAUltt", +"175", +"13", +"Yzfihjlojz"),
         (+"DHAAOBTHTADalojz", +"131", +"10", +"O27"),
         (+"DHJEOZJZAEétépodyz", +"194", +"13", +"Yzfihjlojz"),
         (+"DHJPZAEjoboyyz", +"45", +"5", +"O22"),
         (+"DHJZAVlmozj", +"132", +"10", +"O27"),
         (+"DHXYLTCHAAlkjzty", +"133", +"10", +"O27"),
         (+"DHKJLKAYltbku", +"150", +"11", +"O28"),
         (+"DHKUJLXAPlmop", +"164", +"12", +"O29"),
         (+"DJOTZAAohtza", +"195", +"13", +"Yzfihjlojz"),
         (+"PLQQHXAMotdzty", +"165", +"12", +"O29"),
         (+"PLQQHXAMotdzty", +"165", +"3", +"O20"),
         (+"PLBKXZAYrozjju", +"60", +"6", +"O23"),
         (+"PZFKUTDSAFodrza", +"4", +"3", +"O20"),
         (+"PZJEUXROJZALttl", +"16", +"3", +"O20"),
         (+"PZJOZTTOD-HKJAUARéaètz", +"61", +"6", +"O23"),
         (+"PZXDLFIXAQjépéjod", +"166", +"12", +"O29"),
         (+"PZMZAZUAFljd", +"78", +"7", +"O24"),
         (+"PZMODADlyrzjotz", +"26", +"4", +"O21A-AODX"),
         (+"POLAXZUPOTLAHflj", +"176", +"13", +"Yzfihjlojz"),
         (+"PGZFLPOAJlaop", +"196", +"13", +"Yzfihjlojz"),
         (+"PHTTLPOZKVAZmzautz", +"17", +"3", +"O20"),
         (+"PHJFHUAGzlt-Akd", +"79", +"7", +"O24"),
         (+"PJHKOTALttoz", +"80", +"7", +"O24"),
         (+"PKEHOXAHaomozj", +"62", +"3", +"O20"),
         (+"PKEHOXAHaomozj", +"62", +"6", +"O23"),
         (+"PKYSL-FLAZTAOmlt", +"111", +"9", +"O26"),
         (+"PKNOBAMéjhtowkz", +"63", +"6", +"O23"),
         (+"ZQQLTYOTAFljd", +"151", +"11", +"O28"),
         (+"ZILOAALJPADrljazx", +"197", +"13", +"Yzfihjlojz"),
         (+"ZJRLJPAIlyjods", +"64", +"6", +"O23"),
         (+"QLTZJATlyrlaoz", +"134", +"10", +"O27"),
         (+"QLKAHYALkpjzu", +"198", +"13", +"Yzfihjlojz"),
         (+"QLMZJZLKAPztox", +"96", +"8", +"O25A-AYLOD"),
         (+"QLUHAAZAZjod", +"112", +"9", +"O26"),
         (+"QZEKJOZAGzlt", +"135", +"10", +"O27"),
         (+"QZAYZJADrjoxyolt", +"97", +"8", +"O25A-AYLOD"),
         (+"QHJYOZJAPZAEKTDZUAF.", +"199", +"13", +"Yzfihjlojz"),
         (+"QHKWKZYAYrozjju", +"65", +"6", +"O23"),
         (+"QJLTWKZCLAGklt", +"200", +"13", +"Yzfihjlojz"),
         (+"BLDHTABkoaalkfz", +"167", +"12", +"O29"),
         (+"BLALTALtbéaowkz", +"229", +"13", +"Yzfihjlojz"),
         (+"BLJDOLAZjod", +"136", +"10", +"O27"),
         (+"BLJDOLAPltozal", +"98", +"8", +"O25A-AYLOD"),
         (+"BLJCZTTZADalkpz", +"152", +"11", +"O28"),
         (+"BLKXXHYAGzlt-Iroaoiiz", +"113", +"9", +"O26"),
         (+"BLMZAAZAGzlt-Akd", +"153", +"11", +"O28"),
         (+"BLUJLJPAMotdzty", +"27", +"4", +"O21A-AODX"),
         (+"BZJLJPAAlkjzty", +"201", +"13", +"Yzfihjlojz"),
         (+"BOTHKVAEjkth", +"46", +"5", +"O22"),
         (+"BOTHKVAEjkth", +"46", +"3", +"O20"),
         (+"BOJLJPHYAPhfotowkz", +"137", +"10", +"O27"),
         (+"BHPZQJHUAMotdzty", +"99", +"8", +"O25A-AYLOD"),
         (+"BHUAAlkjzty", +"154", +"11", +"O28"),
         (+"BJLTPBKOAAHY-THUJZYAIlxdlaz", +"114", +"9", +"O26"),
         (+"BJZBHOJZAGzlt-Iozjjz", +"115", +"9", +"O26"),
         (+"BKOAAZJFOTAIlxdla", +"28", +"4", +"O21A-AODX"),
         (+"BKUHTAFljd", +"116", +"9", +"O26"),
         (+"RLLBASjoxyotl", +"203", +"13", +"Yzfihjlojz"),
         (+"RZJUAGzlt-Qjltçhox", +"82", +"7", +"O24"),
         (+"RHLJZLKAQlejodz", +"231", +"13", +"Yzfihjlojz"),
         (+"RHZAAOTBZJAQjltdox", +"204", +"13", +"Yzfihjlojz"),
         (+"RHKYYZFLTZAFojzoaaz", +"232", +"8", +"O25A-AYLOD"),
         (+"OJXLIHKAAZAGzlt-Plmop", +"205", +"13", +"Yzfihjlojz"),
         (+"GLSKEOZDAFljoz-Qjltdz", +"18", +"3", +"O20"),
         (+"GZLTTOTAEaltpotz", +"206", +"13", +"Yzfihjlojz"),
         (+"SRKDAPztox", +"207", +"13", +"Yzfihjlojz"),
         (+"ALDHXYZAPlmop", +"29", +"4", +"O21A-AODX"),
         (+"ALQLJBZAIroaoiiz", +"138", +"10", +"O27"),
         (+"ALQQHTYALaodz", +"83", +"7", +"O24"),
         (+"ALAZKQAGzlt-Dalkpz", +"84", +"7", +"O24"),
         (+"ALF-ROFZAFodrza", +"139", +"10", +"O27"),
         (+"ALFUAGzlt-Xuamzxyjz", +"155", +"11", +"O28"),
         (+"ALJWKZYHKVALttoz", +"19", +"3", +"O20"),
         (+"ALKBOZJAFlyrozk", +"208", +"13", +"Yzfihjlojz"),
         (+"ALKBOZJAQjépéjod", +"168", +"12", +"O29"),
         (+"ALKJZOAALJPAIlxdla", +"85", +"7", +"O24"),
         (+"AZAPZAAOHKAJlufhtpz", +"47", +"5", +"O22"),
         (+"AZDLJIZTYOZJAPlmop", +"156", +"11", +"O28"),
         (+"AZQZEMJZAMotdzty", +"117", +"9", +"O26"),
         (+"AZQHJYAFkjoza", +"209", +"13", +"Yzfihjlojz"),
         (+"AZBZJALalot", +"66", +"6", +"O23"),
         (+"AZBHAAAQjépéjod", +"177", +"13", +"Yzfihjlojz"),
         (+"AZAOZMJZALalot", +"48", +"5", +"O22"),
         (+"AZFZXAZAVlmozj", +"30", +"4", +"O21A-AODX"),
         (+"AZFHJAGzlt-Iroaoiiz", +"183", +"13", +"Yzfihjlojz"),
         (+"AZJHTPZAAPopozj", +"140", +"10", +"O27"),
         (+"AZJHKVABkoaalkfz", +"157", +"11", +"O28"),
         (+"AHJOZYYZAIroaoiiz", +"158", +"11", +"O28"),
         (+"AKDLXAGzlt-Umzx", +"67", +"6", +"O23"),
         (+"AKDLXAFljoz-Aotz", +"230", +"13", +"Yzfihjlojz"),
         (+"FLOXXZAIlxdlaz", +"20", +"3", +"O20"),
         (+"FLAMLBOALttz-Fljoz", +"141", +"10", +"O27"),
         (+"FLTQJZPOAOxlezaaz", +"21", +"3", +"O20"),
         (+"FLJDAJlirlëa", +"86", +"7", +"O24"),
         (+"FLJDZAAQlejodz", +"159", +"11", +"O28"),
         (+"FLJBKZYAXzjbz", +"160", +"11", +"O28"),
         (+"FLJOAEzjtljp", +"49", +"5", +"O22"),
         (+"FLXWKZAOZJAAhux", +"87", +"7", +"O24"),
         (+"FLXXLJYAXéelxyozt", +"178", +"13", +"Yzfihjlojz"),
         (+"FZJUASljof", +"210", +"13", +"Yzfihjlojz"),
         (+"FZKTOZJAXéelxyozt", +"211", +"13", +"Yzfihjlojz"),
         (+"FODRLKALjfzaaz", +"212", +"13", +"Yzfihjlojz"),
         (+"FOALTPJZABztzmoèmzA(DZD)", +"226", +"13", +"Yzfihjlojz"),
         (+"FHTPHYAIroaoiiz", +"179", +"13", +"Yzfihjlojz"),
         (+"FHTQHJYAFljyola", +"31", +"3", +"O20"),
         (+"FHTQHJYAFljyola", +"31", +"4", +"O21A-AODX"),
         (+"FHTOHYABéjljp", +"88", +"7", +"O24"),
         (+"FHJMLTYAHaomozj", +"142", +"10", +"O27"),
         (+"FHKFZTZASrlazp", +"213", +"13", +"Yzfihjlojz"),
         (+"FHKTZUAVlmozj", +"169", +"12", +"O29"),
         (+"FHKJHBHMALazvltpjz", +"143", +"10", +"O27"),
         (+"FHKYHTADrjoxyhirz", +"89", +"7", +"O24"),
         (+"FHKYYLILAIlyjods", +"68", +"6", +"O23"),
         (+"TBKUZTAIRHTBADRLKAIozjjz", +"32", +"4", +"O21A-AODX"),
         (+"TODHALXABéjlap", +"69", +"6", +"O23"),
         (+"TGHRAXlfkza", +"180", +"13", +"Yzfihjlojz"),
         (+"THKLOARLXAEzjtljp", +"90", +"7", +"O24"),
         (+"HEJUAIlxdla", +"102", +"8", +"O25A-AYLOD"),
         (+"HAOMOZJADujoaaz", +"214", +"13", +"Yzfihjlojz"),
         (+"HAOMJUAAlkjzty", +"103", +"8", +"O25A-AYLOD"),
         (+"HKPGLTZATlpol", +"70", +"6", +"O23"),
         (+"ILGHYALaotz", +"33", +"4", +"O21A-AODX"),
         (+"ILHAOAAHAFljyotz", +"118", +"9", +"O26"),
         (+"ILXYHJOTOAXzjbz", +"170", +"12", +"O29"),
         (+"IZBHTALttz-Dalojz", +"215", +"13", +"Yzfihjlojz"),
         (+"IZJJOTAGzlt", +"50", +"5", +"O22"),
         (+"IZYOYAFljd", +"51", +"5", +"O22"),
         (+"IZYOYABkoaalkfz", +"144", +"10", +"O27"),
         (+"IODLJPAGzlt-Qjltçhox", +"71", +"6", +"O23"),
         (+"IODLKAYALttz", +"52", +"5", +"O22"),
         (+"IOZYJZ-DLFELDZPZXAAkphmod", +"34", +"4", +"O21A-AODX"),
         (+"IOAAHTAEzthîy", +"72", +"6", +"O23"),
         (+"IALBTZAAlkjzty", +"119", +"9", +"O26"),
         (+"IHTDHYALtbéaowkz", +"73", +"6", +"O23"),
         (+"IHKAFLJ'DRAZffltkza", +"145", +"10", +"O27"),
         (+"IJHMZTXLAABkoaalkfz", +"216", +"13", +"Yzfihjlojz"),
         (+"WKLYJLOTAJodrljp", +"233", +"8", +"O25A-AYLOD"),
         (+"WKOTTZCAEjkth", +"146", +"10", +"O27"),
         (+"JLTTHKAGkaozt", +"217", +"13", +"Yzfihjlojz"),
         (+"JLXDAZAIlka", +"120", +"9", +"O26"),
         (+"JZTHKQAFojolf", +"218", +"13", +"Yzfihjlojz"),
         (+"JHEZJYAZjod", +"91", +"7", +"O24"),
         (+"JHEOTAFhtowkz", +"1", +"3", +"O20"),
         (+"JHPOZJZALazvltpjz", +"219", +"13", +"Yzfihjlojz"),
         (+"JHXZADrjoxyolt", +"121", +"9", +"O26"),
         (+"JHXXZYAQjltçhox-Plmop", +"171", +"12", +"O29"),
         (+"JHKDLUJHAAQlejodz", +"122", +"9", +"O26"),
         (+"JHKBOZJAGzlt-Zffltkza", +"181", +"13", +"Yzfihjlojz"),
         (+"JHUTZYYZALtyhotz", +"227", +"13", +"Yzfihjlojz"),
         (+"XLTXHTAGzlt-Akd", +"104", +"3", +"O20"),
         (+"XLTXHTAGzlt-Akd", +"104", +"8", +"O25A-AYLOD"),
         (+"XDLMLJPLADrjoxyotz", +"105", +"8", +"O25A-AYLOD"),
         (+"XDROQQFLTTAAupol", +"35", +"4", +"O21A-AODX"),
         (+"XDRFOPALalot", +"92", +"7", +"O24"),
         (+"XDRHZTEZJBZJAIroaoiiz", +"123", +"9", +"O26"),
         (+"XDRKFFALtpjélx", +"74", +"6", +"O23"),
         (+"XDRNLJYCADrjoxyza", +"220", +"13", +"Yzfihjlojz"),
         (+"XDRNLJYCATlpotz", +"147", +"10", +"O27"),
         (+"XDJOFLADrjoxyoltz", +"22", +"3", +"O20"),
         (+"XZBLKPAFljoz", +"221", +"13", +"Yzfihjlojz"),
         (+"XOAMOAQjépéjod", +"36", +"4", +"O21A-AODX"),
         (+"XHOTLJPAVlmozj", +"106", +"8", +"O25A-AYLOD"),
         (+"XHKQQZCAUmzx", +"124", +"9", +"O26"),
         (+"XYLAGzlt-Plmop", +"107", +"8", +"O25A-AYLOD"),
         (+"YLJJLBHALjtlkp", +"37", +"4", +"O21A-AODX"),
         (+"YZJMZJAFodrza", +"234", +"3", +"O20"),
         (+"YZJMZJAFodrza", +"234", +"7", +"O24"),
         (+"YZYLJYAIroaoiiz", +"161", +"11", +"O28"),
         (+"YRLOAMLTAPhfotowkz", +"125", +"9", +"O26"),
         (+"YROELKAYABkoaalkfz", +"93", +"7", +"O24"),
         (+"YHTTHOJAXltpjotz", +"53", +"5", +"O22"),
         (+"YJLMZJXHTAEjkth", +"54", +"5", +"O22"),
         (+"YJOSOAZjod", +"222", +"13", +"Yzfihjlojz"),
         (+"YKGKELAZjfolx", +"223", +"13", +"Yzfihjlojz"),
         (+"YKJEHKAYAQjltçhox", +"75", +"6", +"O23"),
         (+"MLAAZZABzhqqjhu", +"182", +"13", +"Yzfihjlojz"),
         (+"MLAAOZCADujoa", +"224", +"13", +"Yzfihjlojz"),
         (+"MLKPZXDLAAGzlt-Ahkox", +"2", +"3", +"O20"),
         (+"MZJBTZXAGzlt", +"5", +"3", +"O20"),
         (+"MOALOTAPhfotowkz", +"38", +"4", +"O21A-AODX"),
         (+"MOAALJPAGldwkzaotz", +"55", +"5", +"O22"),
         (+"MKAPUAGzlt-Ahkox", +"108", +"8", +"O25A-AYLOD"),
         (+"NLZDSZAAQjltçhoxz", +"162", +"3", +"O20"),
         (+"NLZDSZAAQjltçhoxz", +"162", +"11", +"O28"),
         (+"NLAAZYAXhiroz", +"23", +"3", +"O20"),
         (+"NLJOTAVlmozj", +"76", +"6", +"O23"),
         (+"ULROLHKOAXhkfzulTzxjotz", +"225", +"13", +"Yzfihjlojz"),
         (+"UZXXLULTALtyhotz", +"126", +"9", +"O26"),
         (+"UKLTALalot", +"173", +"12", +"O29"));

   Counter_Id       : Integer;
   List_Lbl         : Vector_Tag;
   List_Id          : Vector_Tag;
   List_First_Gpp   : Vector_Tag;
   List_Group_Id    : Vector_Tag;
   List_Group_Acr   : Vector_Tag;

   People_Group_Id  : Matrix_Tag;
   People_Group_Acr : Matrix_Tag;
   Result           : Translate_Table (1 .. 1);

begin
   Counter_Id := 1;

   for I in People_Set'Range (1) loop

      if Counter_Id > 1 then
         --  Check the id
         if To_String (People_Set (I, 2)) = Item (List_Id, Counter_Id - 1) then
            --  Merge the group id in the same vector for a same
            --  peope id.
            List_Group_Acr := List_Group_Acr & To_String (People_Set (I, 4));
            List_Group_Id  := List_Group_Id  & To_String (People_Set (I, 3));

         else
            --  Insert groups vectors into the matrix

            People_Group_Id  := People_Group_Id & List_Group_Id;
            People_Group_Acr := People_Group_Acr & List_Group_Acr;
            Clear (List_Group_Acr);
            Clear (List_Group_Id);

            List_Group_Acr := List_Group_Acr & To_String (People_Set (I, 4));
            List_Group_Id  := List_Group_Id  & To_String (People_Set (I, 3));
            List_Lbl       := List_Lbl       & To_String (People_Set (I, 1));
            List_Id        := List_Id        & To_String (People_Set (I, 2));
            List_First_Gpp := List_First_Gpp & To_String (People_Set (I, 3));
            Counter_Id := Counter_Id + 1;
         end if;

      else

         --  First item
         List_Group_Acr := List_Group_Acr & To_String (People_Set (I, 4));
         List_Group_Id  := List_Group_Id  & To_String (People_Set (I, 3));
         List_Lbl       := List_Lbl       & To_String (People_Set (I, 1));
         List_Id        := List_Id        & To_String (People_Set (I, 2));
         List_First_Gpp := List_First_Gpp & To_String (People_Set (I, 3));
         Counter_Id := Counter_Id + 1;
      end if;
   end loop;

   --  Last item

   People_Group_Id  := People_Group_Id & List_Group_Id;
   People_Group_Acr := People_Group_Acr & List_Group_Acr;

   Result (1) := Assoc ("GROUP_ACR_M",  People_Group_Acr);

   Put_Line (Parse ("lp.tmplt", Result));
end Tess;
