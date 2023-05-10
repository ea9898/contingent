INSERT INTO ADDRESS_ALLOCATION_ORDERS
            (ID,
             NUMBER,
             NAME,
             OUZ,
             DATE,
             ARCHIVED,
             CREATE_DATE)
VALUES
(2, '2', 'name', NULL, TRUNC(CURRENT_DATE), 0, CURRENT_DATE),
(3, '3', 'name3', NULL, TRUNC(CURRENT_DATE), 0, CURRENT_DATE),
(4, '4', 'name4', NULL, TRUNC(CURRENT_DATE), 1, CURRENT_DATE)
;