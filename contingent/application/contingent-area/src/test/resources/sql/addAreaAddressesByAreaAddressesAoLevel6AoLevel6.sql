INSERT INTO addresses (
            id,
            global_id,
            areacode_omk_te,
            placeCode,
            cityCode,
            aolevel)
VALUES (17072, 10, '0212', '000', '002', '6');

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
VALUES (2, 1, 999, 17072, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);

INSERT INTO areas (
            id,
            mo_id,
            mu_id,
            area_type_code,
            number,
            is_auto_assign_for_attach,
            description,
            attach_by_medical_reason,
            age_min,
            age_max,
            age_m_min,
            age_m_max,
            age_w_min,
            age_w_max,
            create_date,
            update_date,
            archived,
            area_type_profile_code)
VALUES (1, 1, 6485811854, 999, 51, 1, 'филиал5_уч2', null, 18, 150, null, null, null, null,
'2020-02-28 14:56:22.057000', '2020-02-28 14:56:22.057000', 0, null);

INSERT INTO area_addresses (
            id,
            mo_address_id,
            area_id,
            address_id,
            start_date,
            end_date,
            create_date,
            update_date)
VALUES (1, 2, 1, 17072, null, null, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);