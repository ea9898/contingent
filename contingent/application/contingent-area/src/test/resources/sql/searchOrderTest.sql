INSERT INTO ADDRESS_ALLOCATION_ORDERS
            (ID,
             NUMBER,
             NAME,
             OUZ,
             DATE,
             ARCHIVED,
             CREATE_DATE)
VALUES
(2, '2', 'name', NULL, TRUNC(sysdate()), 0, sysdate()),
(3, '3', 'name3', NULL, TRUNC(sysdate()), 0, sysdate()),
(4, '4', 'name4', NULL, TRUNC(sysdate()), 1, sysdate())
;