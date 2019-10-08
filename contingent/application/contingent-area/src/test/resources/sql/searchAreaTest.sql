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
(2, 204, 100, 10, 123, 0, 0, 'Description area 2', 0, NULL, NULL, NULL, NULL, NULL, NULL, sysdate(), sysdate()),
(4, 204, 100, 10, 234, 0, 0, 'Description area 4', 0, NULL, NULL, NULL, NULL, NULL, NULL, sysdate(), sysdate())
;

INSERT INTO AREA_MEDICAL_EMPLOYEES
            (ID,
             MEDICAL_EMPLOYEE_JOB_ID,
             AREA_ID,
             IS_REPLACEMENT,
             START_DATE,
             SNILS,
             POSITION_CODE,
             CREATE_DATE,
             UPDATE_DATE,
             SUBDIVISION_ID)
VALUES
(1, 123, 2, 0, sysdate(), 'snilscode1', '234', sysdate(), sysdate(), 1),
(2, 234, 4, 1, sysdate(), 'snilscode2', '345', sysdate(), sysdate(), 1)
;

INSERT INTO ADDRESSES
            (ID,
             GLOBAL_ID,
             ADDRESS,
             CREATE_DATE,
             UPDATE_DATE)
VALUES
(1, 111, 'Addr 1', sysdate(), sysdate()),
(2, 222, 'Addr 2', sysdate(), sysdate())
;

INSERT INTO AREA_ADDRESSES
            (ID,
             AREA_ID,
             START_DATE,
             ADDRESS_ID,
             CREATE_DATE,
             UPDATE_DATE)
VALUES
(3, 2, sysdate(), 1, sysdate(), sysdate()),
(4, 4, sysdate(), 2, sysdate(), sysdate())
;