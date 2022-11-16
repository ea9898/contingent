INSERT INTO addresses (
            id,
            streetcode,
            areacode_omk_te,
            planCode,
            placeCode,
            cityCode,
            aolevel)
VALUES (17060, '1686', '0212', '0000', '000', '002', '8');

INSERT INTO addresses (
            id,
            streetcode,
            areacode_omk_te,
            planCode,
            placeCode,
            cityCode,
            aolevel)
VALUES (17061, '1686', '0212', '0000', '000', '002', '7');

INSERT INTO addresses (
            id,
            areacode_omk_te,
            planCode,
            placeCode,
            cityCode,
            aolevel)
VALUES (17062, '0212', '0000', '000', '002', '65');

INSERT INTO addresses (
            id,
            areacode_omk_te,
            placeCode,
            cityCode,
            aolevel)
VALUES (17063, '0212', '000', '002', '6');

INSERT INTO addresses (
            id,
            areacode_omk_te,
            cityCode,
            aolevel)
VALUES (17064, '0212', '002', '4');

INSERT INTO addresses (
            id,
            areacode_omk_te,
            aolevel)
VALUES (17065, '0212', '25');

INSERT INTO addresses (
            id,
            region_te_code,
            aolevel)
VALUES (17066, '0800', '2');

INSERT INTO area_types_class (
            code,
            title,
            archived,
            global_id,
            update_date,
            source)
VALUES (1, 'Первичный класс участков', 0, 125723193, TO_TIMESTAMP('2021-06-25 16:14:46.965000', 'YYYY-MM-DD HH24:MI:SS.FF6'), 'syncNsi');

INSERT INTO area_types_kind (
            code,
            title,
            archived,
            global_id,
            update_date,
            source)
VALUES (1, 'Мягко-ассоциированный участок', 0, 125694438, TO_TIMESTAMP('2021-06-25 16:14:47.278000', 'YYYY-MM-DD HH24:MI:SS.FF6'), 'syncNsi');

INSERT INTO area_type (
            code,
            title,
            area_type_kind_code,
            area_type_class_code,
            head_finance,
            attach_by_medical_reason,
            mpgu_available,
            area_count_limit_code,
            gender_code,
            residents_bind_rate,
            age_min,
            age_max,
            age_m_min,
            age_m_max,
            age_w_min,
            age_w_max,
            archived,
            global_id,
            update_date,
            source)
VALUES (999, 'тестовый пуш', 1, 1, NULL , NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 1, 125721017,
TO_TIMESTAMP('2020-01-23 10:19:14.045376', 'YYYY-MM-DD HH24:MI:SS.FF6'), NULL);

INSERT INTO mo_addresses (
            id,
            mo_id,
            area_type_code,
            address_id,
            create_date,
            end_date)
VALUES (5L, -2202860706, 999, 17060, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);

INSERT INTO mo_addresses (
            id,
            mo_id,
            area_type_code,
            address_id,
            create_date,
            end_date)
VALUES (6L, -2202860706, 999, 17061, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);

INSERT INTO mo_addresses (
            id,
            mo_id,
            area_type_code,
            address_id,
            create_date,
            end_date)
VALUES (7L, -2202860706, 999, 17062, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);

INSERT INTO mo_addresses (
            id,
            mo_id,
            area_type_code,
            address_id,
            create_date,
            end_date)
VALUES (8L, -2202860706, 999, 17063, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);

INSERT INTO mo_addresses (
            id,
            mo_id,
            area_type_code,
            address_id,
            create_date,
            end_date)
VALUES (9L, -2202860706, 999, 17064, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);

INSERT INTO mo_addresses (
            id,
            mo_id,
            area_type_code,
            address_id,
            create_date,
            end_date)
VALUES (10L, -2202860706, 999, 17065, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);

INSERT INTO mo_addresses (
            id,
            mo_id,
            area_type_code,
            address_id,
            create_date,
            end_date)
VALUES (11L, -2202860706, 999, 17066, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);