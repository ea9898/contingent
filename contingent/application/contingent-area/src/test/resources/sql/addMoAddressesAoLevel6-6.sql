INSERT INTO addresses (
            id,
            global_id,
            areacode_omk_te,
            placeCode,
            cityCode,
            aolevel)
VALUES (3, 30, '0212', '000', '002', '6');

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
VALUES (199, 'тестовый пуш', 1, 1, NULL , NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, 125721018,
TO_TIMESTAMP('2020-01-23 10:19:14.045376', 'YYYY-MM-DD HH24:MI:SS.FF6'), NULL);

INSERT INTO address_allocation_orders (
            id,
            number,
            name,
            date,
            ouz,
            create_date,
            update_date,
            archived)
VALUES (1, 93691, 'Name.Order.Update', TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), 'OUZ.Update', TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), 0);

INSERT INTO mo_addresses (
            id,
            mo_id,
            area_type_code,
            order_id,
            address_id,
            create_date,
            end_date)
VALUES (1, 20, 199, 1, 3, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);
