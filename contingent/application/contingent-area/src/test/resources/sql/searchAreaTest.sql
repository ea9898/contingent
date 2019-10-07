INSERT INTO AREA_TYPES_CLASS
            (CODE,
             TITLE,
             ARCHIVED,
             GLOBAL_ID)
VALUES
(1, 'Primary test', 0, 11),
(2, 'Dependent test', 0, 22)
;

INSERT INTO AREA_TYPES_KIND
            (CODE,
             TITLE,
             ARCHIVED,
             GLOBAL_ID)
VALUES
(1, 'Test kind 1', 0, 111)
;

INSERT INTO AREA_TYPE
            (CODE,
            TITLE,
            AREA_TYPE_KIND_CODE,
            AREA_TYPE_CLASS_CODE,
            GENDER_CODE,
            AGE_MIN,
            AGE_MAX,
            AGE_M_MIN,
            AGE_M_MAX,
            AGE_W_MIN,
            AGE_W_MAX,
            ARCHIVED,
            HEAD_FINANCE,
            HAS_SERVICE_TERRITORY,
            ATTACH_BY_REQUEST,
            ATTACH_BY_MEDICAL_REASON,
            MPGU_AVAILABLE,
            AREA_COUNT_LIMIT_CODE,
            RESIDENTS_BIND_RATE,
            GLOBAL_ID)
VALUES
(10, 'Area type Primary', 1, 1, 1, NULL, NULL, NULL, NULL, NULL, NULL, 0, 1, 0, 0, 0, 0, NULL, 100, 11111)
;

INSERT INTO AREAS
            (ID,
             MO_ID,
             MU_ID,
             AREA_TYPE_CODE,
             NUMBER,
             IS_AUTO_ASSIGN_FOR_ATTACH,
             ARCHIVED,
             DESCRIPTION,
             ATTACH_BY_MEDICAL_REASON,
             AGE_MIN,
             AGE_MAX,
             AGE_M_MIN,
             AGE_M_MAX,
             AGE_W_MIN,
             AGE_W_MAX,
             CREATE_DATE,
             UPDATE_DATE)
VALUES
(2, 204, 100, 10, 123, 0, 0, 'Description area 2', 0, NULL, NULL, NULL, NULL, NULL, NULL, sysdate(), sysdate())
;