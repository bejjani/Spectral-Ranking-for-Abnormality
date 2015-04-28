CREATE OR REPLACE FUNCTION IDD.S800014_PARALLEL_SADC_EXP_FNC ( 
                  p_source    IN SYS_REFCURSOR, 
                  p_filename  IN VARCHAR2, 
                  p_directory IN VARCHAR2 
                  ) RETURN dump_ntt 
                    PIPELINED 
                    PARALLEL_ENABLE (PARTITION p_source BY ANY) 
AS 
-- ----------------------------------------------------------------------------- 
-- Nom         : S800014_PARALLEL_SADC_EXP_FNC 
-- Auteur      : Christophe Bejjani 
-- Date        : 2012/05/10 
-- Description : Ecritures du contenu du cursor passe en parametre dans un fichier plat 
--               Cette fontion est pipelined et parallel_enabled permettant de repartir 
--               la tache d'exportation du cursor a differentes instances de cette fonction 
--               La procedure S800012_IDD_SADC_EXP_SP appelle cette fontion 
--                
-- 
-- Paramètres  : p_source: cursor 
--               p_filename: nom fichier 
--               p_directory: oracle logical directory 
-- 
-- Commentaires:  
--                
-- ----------------------------------------------------------------------------- 
     TYPE row_ntt IS TABLE OF VARCHAR2(32767); 
     v_rows    row_ntt; 
     v_file    UTL_FILE.FILE_TYPE; 
     v_buffer  VARCHAR2(32767); 
     v_sid     NUMBER; 
     v_name    VARCHAR2(128); 
     v_lines   PLS_INTEGER := 0; 
     c_eol     CONSTANT VARCHAR2(1) := CHR(10); 
     c_eollen  CONSTANT PLS_INTEGER := LENGTH(c_eol); 
     c_maxline CONSTANT PLS_INTEGER := 32767; 
 
BEGIN 
 
     SELECT sid INTO v_sid FROM v$mystat WHERE ROWNUM = 1; 
      
     v_name := p_filename || '_' || TO_CHAR(v_sid); 
     v_file := UTL_FILE.FOPEN(p_directory, v_name || '.tmp', 'w', 32767); 
 
     LOOP 
       FETCH p_source BULK COLLECT INTO v_rows LIMIT 100; 
 
        FOR i IN 1 .. v_rows.COUNT LOOP 
 
           IF LENGTH(v_buffer) + c_eollen + LENGTH(v_rows(i)) <= c_maxline THEN 
              v_buffer := v_buffer || c_eol || v_rows(i); 
           ELSE 
              IF v_buffer IS NOT NULL THEN 
                 UTL_FILE.PUT_LINE(v_file, v_buffer); 
              END IF; 
              v_buffer := v_rows(i); 
           END IF; 
 
        END LOOP; 
 
        v_lines := v_lines + v_rows.COUNT; 
 
        EXIT WHEN p_source%NOTFOUND; 
     END LOOP; 
     CLOSE p_source; 
     
     IF v_buffer IS NOT NULL THEN 
         UTL_FILE.PUT_LINE(v_file, v_buffer); 
     END IF; 
     
     UTL_FILE.FCLOSE(v_file); 
     UTL_FILE.FRENAME (p_directory, v_name || '.tmp', p_directory, v_name, FALSE); 
      
     PIPE ROW (dump_ot(v_name, v_lines, v_sid)); 
     RETURN; 
 
END S800014_PARALLEL_SADC_EXP_FNC;
/
