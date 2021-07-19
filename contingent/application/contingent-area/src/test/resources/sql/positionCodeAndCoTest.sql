INSERT INTO POSITION_CODE
            (CODE,
             GLOBAL_ID,
             NOM_TYPE,
             SERIAL_NUM,
             CONSTANT_TITLE,
             UPDATE_DATE)
VALUES
('test1', 111, 'type', 0, 'test 1', sysdate()),
('test2', 222, 'type', 1, 'test 2', sysdate())
;
INSERT INTO POSITION_NOM
            (GLOBAL_ID,
             TITLE,
             POSITION_CODE_ID,
             START_DATE,
             SPECIALIZATION_ID,
             UPDATE_DATE)
VALUES
(11, 'test_1', 111, TO_DATE('02-04-2018', 'DD-MM-YYYY'), 1, sysdate()),
(22, 'test_2', 222, TO_DATE('02-04-2018', 'DD-MM-YYYY'), 2, sysdate())
;
INSERT INTO AREA_TYPE_MEDICAL_POSITIONS
            (GLOBAL_ID,
             AREA_TYPE_CODE,
             POSITION_CODE,
             ARCHIVED,
             UPDATE_DATE)
VALUES
(1, 10, 'test1', 0, sysdate()),
(2, 10, 'test2', 0, sysdate())
;
INSERT INTO POSITION_SUPP
            (GLOBAL_ID,
             CODE,
             TITLE_SHORT,
             PARENT_ID,
             UPDATE_DATE)
VALUES
(111222, '135', 'short title 1', NULL, sysdate())
;
INSERT INTO MAPPING_POSITIONCODE_TO_OTHERPOSITION
            (GLOBAL_ID,
             POSITIONCODE_ID,
             EGISZWORKPOSITION_ID,
             PS_GLOBAL_ID,
             UPDATE_DATE)
VALUES
(13, 111, 12345, 111222, sysdate())
;
ALTER SEQUENCE seq_area_medical_employee RESTART WITH 100;
