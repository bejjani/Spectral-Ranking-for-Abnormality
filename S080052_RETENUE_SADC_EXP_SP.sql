CREATE OR REPLACE PROCEDURE IDD.S080052_RETENUE_SADC_EXP_SP (
    p_PARAM IN VARCHAR2
)
IS
-- -----------------------------------------------------------------------------
-- Nom         : S080052_RETENUE_SADC_EXP_SP
-- Auteur      : Christophe Bejjani
-- Date        : 2012/05/10
-- Description : Ecriture de la table IDD_0800_RETENUE_SADC dans les fichiers
--               
-- Paramètres  : ID_SOUS_SYS, ECHAN DEFAULT 0
-- 
-- Commentaires: Un fichier par combinaison ID_ENTITE_LEGALE, ID_SOUS_SYST tel
--               configure dans la table SAD_ENTT_SSYST
-- -----------------------------------------------------------------------------
   v_nom_procedure   VARCHAR2 (40)   := 'S080052_RETENUE_SADC_EXP_SP';
   v_val_parametre   VARCHAR2 (80)   := p_PARAM;             -- Max 80 char
   v_val_erreur      VARCHAR2 (2000) := NULL;                -- Max 2000 char
   v_nb_sel          NUMBER          := 0;
   v_nb_ins          NUMBER          := 0;
   v_nb_maj          NUMBER          := 0;
   v_nb_detruits     NUMBER          := 0;
   
   v_separator        VARCHAR2(5);
   v_concat           VARCHAR2(5);
   v_quote            VARCHAR2(5);
   v_table_name       VARCHAR2(30)    := 'IDD_0800_RETENUE_SADC';
   v_header           VARCHAR(4000);
   v_stmnt            VARCHAR(4000);
   l_stmt             LONG;
   DirLoc             VARCHAR2(30);
   sll_rec            VARCHAR2(200);
   TYPE CurTyp IS REF CURSOR;
   sll_cur            CurTyp;
   v_File             utl_file.file_type;
   v_file_name        VARCHAR2(100);
   v_date             DATE;
   l_rec              dump_ot;
   
   p_ID_SOUS_SYS      NUMBER;
   p_ECHAN            PLS_INTEGER;
    
    CURSOR header_cur (in_table_name IN VARCHAR2) IS
     SELECT NOMCOL_FR, NOMCOL_EN
     FROM   IDD_EXPORT_CONF
     WHERE  NOM_TABLE = in_table_name
     AND    AFFICHABLE = 1
     ORDER BY ORDRE_AFFICHAGE ASC;
    
    CURSOR entite_cur IS
     SELECT ID_ENTITE_LEGALE, TO_CHAR(ID_SOUS_SYST,'FM000') ID_SOUS_SYST
     FROM SAD_ENTT_SSYST WHERE ID_SOUS_SYST = p_ID_SOUS_SYS;
    
BEGIN

    p_ID_SOUS_SYS   := Split_Param_Fnc(P_PARAM,1);
    p_ECHAN         := NVL(Split_Param_Fnc(P_PARAM,2),0);
    
    dbms_session.set_nls('NLS_NUMERIC_CHARACTERS', '''. ''');

    FOR REC_entite IN entite_cur
    LOOP
        dbms_output.put_line('Entite Legale: ' || REC_entite.ID_ENTITE_LEGALE);
        
        CASE p_ECHAN
            WHEN 0 THEN DirLoc := 'EXP_ENT_';
            WHEN 1  THEN DirLoc := 'EXP_ECHAN_ENT_';
        END CASE;
        DirLoc := DirLoc || TO_CHAR(REC_entite.ID_ENTITE_LEGALE, 'FM09') || REC_entite.ID_SOUS_SYST;

        --Generate file name
        SELECT NO_IDENT_SADC || TO_CHAR(SYSDATE, 'YYYYMMDDHH24MISS') || SUBSTR(v_table_name,5,4) INTO v_file_name
        FROM SAD_ENTT_LGLE
        WHERE ID_ENTITE_LEGALE = REC_entite.ID_ENTITE_LEGALE;
        
        BEGIN
            SELECT v_file_name || METHODE_EXTRACTION || TO_CHAR(VERSION_SADC,'FM000') || TO_CHAR(DECODE(METHODE_EXTRACTION, 2, REC_entite.ID_SOUS_SYST, 1, 0), 'FM000') INTO v_file_name
            FROM SAD_INFO_SADC
            WHERE ID_ENTITE_LEGALE = REC_entite.ID_ENTITE_LEGALE;
        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                RAISE_APPLICATION_ERROR(-20211, v_nom_PROCEDURE || ' Methode d''extraction non definie dans SAD_INFO_SADC pour ID_ENTITE_LEGALE ' || REC_entite.ID_ENTITE_LEGALE);
        END;
        
        dbms_output.put_line('   ' || v_file_name);
        
        
        
        v_stmnt     := NULL;
        v_header    := NULL;
        v_separator := NULL;
        v_concat    := NULL;
        v_quote     := NULL;
        v_stmnt     := 'SELECT /*+ PARALLEL(s,4) */ ';
        --Genrate the header and the Select statemenet
        FOR REC_header in header_cur (v_table_name)
        LOOP
            v_stmnt  := v_stmnt || v_concat || v_quote || v_separator || v_quote || v_concat || REC_header.NOMCOL_FR;
            v_header := v_header || v_separator || REC_header.NOMCOL_EN;
            v_separator := '|';
            v_concat    := '||';
            v_quote     := '''';
        END LOOP;
        
        --Write the records in parallel by calling the pipelined parallel_enabled function S800014_PARALLEL_SADC_EXP_FNC
        v_stmnt := v_stmnt || ' FROM ' || v_table_name || ' s WHERE ID_ENTITE_LEGALE = ' || REC_entite.id_entite_legale || ' AND ID_SOUS_SYST = ' || REC_entite.id_sous_syst;
        CASE p_ECHAN
            WHEN 0 THEN v_stmnt := v_stmnt;
            WHEN 1  THEN v_stmnt := v_stmnt || ' AND IND_ECHANTILLONNAGE = 1';
        END CASE;
        l_stmt := ('SELECT DUMP_OT(file_name,no_records,session_id) FROM TABLE(S800014_PARALLEL_SADC_EXP_FNC(CURSOR(' || v_stmnt ||
                          '), ''' || v_file_name || ''', ''' || DirLoc || ''')) nt');
        dbms_output.put_line(l_stmt);
            
        v_nb_sel:=0;                         
        OPEN sll_cur FOR l_stmt;
        LOOP
            FETCH sll_cur INTO l_rec;
            EXIT WHEN sll_cur%NOTFOUND;
            v_nb_sel:=v_nb_sel+l_rec.no_records;
            dbms_output.put_line( l_rec.file_name || ' genere avec ' || l_rec.no_records || ' records');
        END LOOP;
        CLOSE sll_cur;
            
        --Write the header file
        v_File := UTL_FILE.FOPEN(DirLoc, v_file_name || '.tmp', 'w', 32767);
        UTL_FILE.PUT_LINE(v_File, v_header);
        UTL_FILE.FCLOSE(v_File);
        UTL_FILE.FRENAME (DirLoc, v_file_name || '.tmp', DirLoc, v_file_name, FALSE);
        dbms_output.put_line( v_file_name || ' genere avec le record de header');
        dbms_output.put_line('v_nb_SEL: ' || to_char(v_nb_sel) || '; v_nb_INS: ' || to_char(v_nb_ins) ||'; v_nb_MAJ: ' || to_char(v_nb_maj));

    END LOOP;
      
    MAJ_STATUT_CMPS_PROC_SP (
              v_nom_PROCEDURE,
              v_val_PARAMETRE,
              v_val_ERREUR,
              v_nb_SEL,
              v_nb_INS,
              v_nb_MAJ,
              v_nb_DETRUITS);

EXCEPTION
    WHEN OTHERS THEN
   
        MAJ_STATUT_CMPS_PROC_SP (
            v_nom_PROCEDURE,
            v_val_PARAMETRE,
            SQLERRM,
            v_nb_SEL,
            v_nb_INS,
            v_nb_MAJ,
            v_nb_DETRUITS);
            
        RAISE_APPLICATION_ERROR(-20211,v_nom_PROCEDURE || ' erreur : ' || SQLERRM);
        
        dbms_output.put_line(SQLERRM);

END S080052_RETENUE_SADC_EXP_SP;
/
