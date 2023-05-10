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
(10, 'Area type Primary', 1, 1, 1, NULL, NULL, NULL, NULL, NULL, NULL, 0, 1, 0, 0, 0, 0, NULL, 100, 11111),
(20, 'Area type Secondary', 1, 2, 1, NULL, NULL, NULL, NULL, NULL, NULL, 0, 1, 0, 0, 0, 0, NULL, 100, 11112)
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
(2, 204, 100, 10, 123, 0, 0, 'Description area 2', 0, NULL, NULL, NULL, NULL, NULL, NULL, CURRENT_DATE, CURRENT_DATE),
(4, 204, 100, 10, 234, 0, 0, 'Description area 4', 0, NULL, NULL, NULL, NULL, NULL, NULL, CURRENT_DATE, CURRENT_DATE),
(8, 204, 100, 20, 238, 0, 0, 'Description area 8', 0, NULL, NULL, NULL, NULL, NULL, NULL, CURRENT_DATE, CURRENT_DATE),
(10, 136, 100, 20, 240, 0, 0, 'Description area 10', 0, NULL, NULL, NULL, NULL, NULL, NULL, CURRENT_DATE, CURRENT_DATE),
(12, 136, 100, 20, 242, 0, 0, 'Description area 12', 0, NULL, NULL, NULL, NULL, NULL, NULL, CURRENT_DATE, CURRENT_DATE)
;

INSERT INTO AREA_MU_SERVICE
            (ID,
            AREA_ID,
            MU_ID,
            START_DATE,
            END_DATE
            )
VALUES
(1, 2, 100, TO_DATE('02-04-2018', 'DD-MM-YYYY'), null),
(2, 4, 100, TO_DATE('02-04-2018', 'DD-MM-YYYY'), null),
(3, 10, 100, TO_DATE('02-04-2018', 'DD-MM-YYYY'), null),
(4, 12, 100, TO_DATE('02-04-2018', 'DD-MM-YYYY'), TO_DATE('03-04-2018', 'DD-MM-YYYY'))
;

INSERT INTO AREA_MEDICAL_EMPLOYEES
            (ID,
             MEDICAL_EMPLOYEE_JOB_ID,
             AREA_ID,
             IS_REPLACEMENT,
             START_DATE,
             END_DATE,
             SNILS,
             POSITION_CODE,
             CREATE_DATE,
             UPDATE_DATE,
             SUBDIVISION_ID)
VALUES
(1, 123, 2, 0, CURRENT_DATE, NULL, 'snilscode1', '234', CURRENT_DATE, CURRENT_DATE, 1),
(2, 234, 4, 1, CURRENT_DATE, NULL, 'snilscode2', '345', CURRENT_DATE, CURRENT_DATE, 1),
(3, 235, 8, 0, CURRENT_DATE, NULL, 'snilscode3', '346', CURRENT_DATE, CURRENT_DATE, 1),
(4, 236, 10, 0, TO_DATE('02-04-2018', 'DD-MM-YYYY'), TO_DATE('02-04-2019', 'DD-MM-YYYY'), 'snilscode4', '347', CURRENT_DATE, CURRENT_DATE, 1),
(5, 237, 12, 0, TO_DATE('02-04-2018', 'DD-MM-YYYY'), TO_DATE('02-04-2040', 'DD-MM-YYYY'), 'snilscode5', '348', CURRENT_DATE, CURRENT_DATE, 1)
;

INSERT INTO ADDRESSES
            (ID,
             GLOBAL_ID,
             ADDRESS,
             CREATE_DATE,
             UPDATE_DATE,
             REGION_TE_ID)
VALUES
(-1, 111, 'Addr 1', CURRENT_DATE, CURRENT_DATE, 1),
(-2, 222, 'Addr 2', CURRENT_DATE, CURRENT_DATE, 2)
;

INSERT INTO AREA_ADDRESSES
            (ID,
             AREA_ID,
             START_DATE,
             ADDRESS_ID,
             CREATE_DATE,
             UPDATE_DATE)
VALUES
(3, 2, CURRENT_DATE, -1, CURRENT_DATE, CURRENT_DATE),
(4, 4, CURRENT_DATE, -2, CURRENT_DATE, CURRENT_DATE)
;

INSERT INTO SPECIALIZATION
            (CODE,
             TITLE,
             ARCHIVED,
             GLOBAL_ID,
             UPDATE_DATE,
             SOURCE)
VALUES
(49, 'SPEC 1', 0, 1, CURRENT_DATE, 's'),
(76, 'SPEC 2', 0, 2, CURRENT_DATE, 's')
;

INSERT INTO AREA_TYPE_SPECIALIZATIONS
            (GLOBAL_ID,
             AREA_TYPE_CODE,
             SPECIALIZATION_CODE,
             ARCHIVED,
             UPDATE_DATE,
             SOURCE)
VALUES
(123, 10, 49, 0, CURRENT_DATE, 's'),
(124, 20, 76, 0, CURRENT_DATE, 's')
;
