INSERT INTO addresses (
            id,
            global_id,
            areacode_omk_te,
            aolevel)
VALUES (17074, 10, '0212', '25');

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
VALUES (2, 1, 999, 17074, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);

INSERT INTO areas (
            id,
            mo_id,
            mu_id,
            area_type_code,
            number,
            special_number,
--             area_type_category,
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
            area_type_profile_code,
            att_final_limit,
            att_info_limit
--             residents_bind_rate
            )
VALUES (1, 1, 6485811854, 999, 51, '111', /*222,*/ 1, 'филиал5_уч2', null, 18, 150, null, null, null, null,
'2020-02-28 14:56:22.057000', '2020-02-28 14:56:22.057000', 0, null, 333, 444/*, 555*/);

INSERT INTO area_medical_employees (
                                    id,
                                    medical_employee_job_id,
                                    area_id,
                                    is_replacement,
                                    start_date,
                                    end_date,
                                    snils,
                                    position_code,
                                    subdivision_id,
                                    create_date,
                                    update_date,
                                    is_error,
                                    position_code_supp,
                                    employee_category)
VALUES (57885, 11354096, 1, 0, '2021-07-07 00:00:00.000000', null, '07126017324',
        'AMB_MED_264', 13950553, '2021-07-06 16:41:48.632000', '2021-07-06 16:41:48.632000', 0, null, 0);

INSERT INTO area_addresses (
            id,
            mo_address_id,
            area_id,
            address_id,
            start_date,
            end_date,
            create_date,
            update_date)
VALUES (1, 2, 1, 17074, null, null, TO_TIMESTAMP('2020-09-24 11:42:27.668000', 'YYYY-MM-DD HH24:MI:SS.FF6'), null);

INSERT INTO medical_organisations_onko (global_id, id, id_rmu, id_smvr2, name, type, mo_id, code_onco_area, archived, update_date, source) VALUES (1111, 1, 1, 1, 'тест', '1', 1, '123', 0, '2023-06-26 13:02:21.000000', 'test');
