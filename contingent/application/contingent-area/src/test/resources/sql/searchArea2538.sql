INSERT INTO address_allocation_orders (id, number, name, date, ouz, create_date, update_date, archived) VALUES (4, '1', 'Фактическое распределение территории обслуживания по состоянию на момент миграции в К2', '1900-01-01 00:00:00.000000', 'Фактическое распределение территории обслуживания по состоянию на момент миграции в К2', '2019-11-18 00:00:00.000000', '2019-11-18 00:00:00.000000', 0);

INSERT INTO area_types_class (code, title, archived, global_id, update_date, source) VALUES (222, 'Первичный класс участков', 0, 125723193, TO_TIMESTAMP('2021-06-25 16:14:46.965000', 'YYYY-MM-DD HH24:MI:SS.FF6'), 'syncNsi');

INSERT INTO area_types_kind (code, title, archived, global_id, update_date, source) VALUES (1, 'Мягко-ассоциированный участок', 0, 125694438, TO_TIMESTAMP('2021-06-25 16:14:47.278000', 'YYYY-MM-DD HH24:MI:SS.FF6'), 'syncNsi');

INSERT INTO area_type (code, title, area_type_kind_code, area_type_class_code, head_finance, attach_by_medical_reason, mpgu_available, area_count_limit_code, gender_code, residents_bind_rate, age_min, age_max, age_m_min, age_m_max, age_w_min, age_w_max, archived, global_id, update_date, source) VALUES (10, 'тестовый пуш', 1, 1, NULL , NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 1, 125721010, TO_TIMESTAMP('2020-01-23 10:19:14.045376', 'YYYY-MM-DD HH24:MI:SS.FF6'), NULL);

INSERT INTO area_type (code, title, area_type_kind_code, area_type_class_code, head_finance, attach_by_medical_reason, mpgu_available, area_count_limit_code, gender_code, residents_bind_rate, age_min, age_max, age_m_min, age_m_max, age_w_min, age_w_max, archived, global_id, update_date, source) VALUES (20, 'тестовый пуш', 1, 1, NULL , NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, 1, 125721020, TO_TIMESTAMP('2020-01-23 10:19:14.045376', 'YYYY-MM-DD HH24:MI:SS.FF6'), NULL);

INSERT INTO areas (id, mo_id, mu_id, area_type_code, number, is_auto_assign_for_attach, description, attach_by_medical_reason, age_min, age_max, age_m_min, age_m_max, age_w_min, age_w_max, create_date, update_date, archived, area_type_profile_code) VALUES (127, 10310805, -333, 10, 1, 0, 'Участок №1 ', null, null, null, null, null, null, null, '2020-07-08 01:11:18.207514', '2021-09-03 11:50:24.945000', 0, null);

INSERT INTO addresses (id, create_date, aolevel, global_id, region_te_code, region_te_name, region_te_typename, region_te_typename_short, areacode_omk_te, area_te_name, area_te_typename, area_te_typename_short, areacode, area_bti_code, area_name, area_typename, area_typename_short, citycode, city_bti_code, city_name, city_typename, city_typename_short, placecode, place_bti_code, place_name, place_typename, place_typename_short, plancode, plan_bti_code, plan_name, plan_typename, plan_typename_short, streetcode, street_name, street_omk_um, street_typename, street_typename_short, street_bti_code, l1_type, l1_type_short, l1_value, l2_type, l2_type_short, l2_value, l3_type, l3_type_short, l3_value, address, update_date, region_id, region_code, region_name, region_te_id, region_typename, area_te_id, area_id, city_id, place_id, plan_id, street_id) VALUES (11420, '2020-07-08 02:07:31.301396', '8', 68782073, '0200', 'Северный', 'административный округ', null, '0211', 'Левобережный', 'муниципальный округ', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, '0836', 'Беломорская', '001640', 'Улица', 'ул.', '193957', 'дом', 'д.', '11', 'корпус', 'к.', '2', null, null, null, 'город Москва, улица Беломорская, дом 11, корпус 2', '2020-07-22 00:09:01.476000', 67200856, '77', 'Москва', '67186391', 'Город', '67186443', null, null, null, null, 67822129);
INSERT INTO addresses (id, create_date, aolevel, global_id, region_te_code, region_te_name, region_te_typename, region_te_typename_short, areacode_omk_te, area_te_name, area_te_typename, area_te_typename_short, areacode, area_bti_code, area_name, area_typename, area_typename_short, citycode, city_bti_code, city_name, city_typename, city_typename_short, placecode, place_bti_code, place_name, place_typename, place_typename_short, plancode, plan_bti_code, plan_name, plan_typename, plan_typename_short, streetcode, street_name, street_omk_um, street_typename, street_typename_short, street_bti_code, l1_type, l1_type_short, l1_value, l2_type, l2_type_short, l2_value, l3_type, l3_type_short, l3_value, address, update_date, region_id, region_code, region_name, region_te_id, region_typename, area_te_id, area_id, city_id, place_id, plan_id, street_id) VALUES (12903, '2020-07-08 02:07:31.301396', '8', 71382054, '0200', 'Северный', 'административный округ', null, '0211', 'Левобережный', 'муниципальный округ', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, '2682', 'Смольная', '025580', 'Улица', 'ул.', '197015', 'дом', 'д.', '61', 'корпус', 'к.', '1', null, null, null, 'город Москва, улица Смольная, дом 61, корпус 1', '2020-07-22 00:19:58.581000', 67200856, '77', 'Москва', '67186391', 'Город', '67186443', null, null, null, null, 67515190);
INSERT INTO addresses (id, create_date, aolevel, global_id, region_te_code, region_te_name, region_te_typename, region_te_typename_short, areacode_omk_te, area_te_name, area_te_typename, area_te_typename_short, areacode, area_bti_code, area_name, area_typename, area_typename_short, citycode, city_bti_code, city_name, city_typename, city_typename_short, placecode, place_bti_code, place_name, place_typename, place_typename_short, plancode, plan_bti_code, plan_name, plan_typename, plan_typename_short, streetcode, street_name, street_omk_um, street_typename, street_typename_short, street_bti_code, l1_type, l1_type_short, l1_value, l2_type, l2_type_short, l2_value, l3_type, l3_type_short, l3_value, address, update_date, region_id, region_code, region_name, region_te_id, region_typename, area_te_id, area_id, city_id, place_id, plan_id, street_id) VALUES (13940, '2020-07-08 02:07:31.301396', '8', 73259251, '0200', 'Северный', 'административный округ', null, '0211', 'Левобережный', 'муниципальный округ', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, '2682', 'Смольная', '025580', 'Улица', 'ул.', '197015', 'дом', 'д.', '63', null, null, null, null, null, null, 'город Москва, улица Смольная, дом 63', '2020-07-22 00:45:14.787000', 67200856, '77', 'Москва', '67186391', 'Город', '67186443', null, null, null, null, 67515190);
INSERT INTO addresses (id, create_date, aolevel, global_id, region_te_code, region_te_name, region_te_typename, region_te_typename_short, areacode_omk_te, area_te_name, area_te_typename, area_te_typename_short, areacode, area_bti_code, area_name, area_typename, area_typename_short, citycode, city_bti_code, city_name, city_typename, city_typename_short, placecode, place_bti_code, place_name, place_typename, place_typename_short, plancode, plan_bti_code, plan_name, plan_typename, plan_typename_short, streetcode, street_name, street_omk_um, street_typename, street_typename_short, street_bti_code, l1_type, l1_type_short, l1_value, l2_type, l2_type_short, l2_value, l3_type, l3_type_short, l3_value, address, update_date, region_id, region_code, region_name, region_te_id, region_typename, area_te_id, area_id, city_id, place_id, plan_id, street_id) VALUES (16951, '2020-07-08 02:07:31.301396', '8', 78617198, '0200', 'Северный', 'административный округ', null, '0211', 'Левобережный', 'муниципальный округ', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, '2682', 'Смольная', '025580', 'Улица', 'ул.', '197015', 'дом', 'д.', '44', 'корпус', 'к.', '1', null, null, null, 'город Москва, Смольная улица, дом 44, корпус 1', '2020-07-22 00:17:46.007000', 67200856, '77', 'Москва', '67186391', 'Город', '67186443', null, null, null, null, 67515190);
INSERT INTO addresses (id, create_date, aolevel, global_id, region_te_code, region_te_name, region_te_typename, region_te_typename_short, areacode_omk_te, area_te_name, area_te_typename, area_te_typename_short, areacode, area_bti_code, area_name, area_typename, area_typename_short, citycode, city_bti_code, city_name, city_typename, city_typename_short, placecode, place_bti_code, place_name, place_typename, place_typename_short, plancode, plan_bti_code, plan_name, plan_typename, plan_typename_short, streetcode, street_name, street_omk_um, street_typename, street_typename_short, street_bti_code, l1_type, l1_type_short, l1_value, l2_type, l2_type_short, l2_value, l3_type, l3_type_short, l3_value, address, update_date, region_id, region_code, region_name, region_te_id, region_typename, area_te_id, area_id, city_id, place_id, plan_id, street_id) VALUES (24541, '2020-07-08 02:07:31.301396', '8', 91989290, '0200', 'Северный', 'административный округ', null, '0211', 'Левобережный', 'муниципальный округ', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, '2682', 'Смольная', '025580', 'Улица', 'ул.', '197015', 'дом', 'д.', '44', 'корпус', 'к.', '2', null, null, null, 'город Москва, улица Смольная, дом 44, корпус 2', '2020-07-21 23:50:05.200000', 67200856, '77', 'Москва', '67186391', 'Город', '67186443', null, null, null, null, 67515190);
INSERT INTO addresses (id, create_date, aolevel, global_id, region_te_code, region_te_name, region_te_typename, region_te_typename_short, areacode_omk_te, area_te_name, area_te_typename, area_te_typename_short, areacode, area_bti_code, area_name, area_typename, area_typename_short, citycode, city_bti_code, city_name, city_typename, city_typename_short, placecode, place_bti_code, place_name, place_typename, place_typename_short, plancode, plan_bti_code, plan_name, plan_typename, plan_typename_short, streetcode, street_name, street_omk_um, street_typename, street_typename_short, street_bti_code, l1_type, l1_type_short, l1_value, l2_type, l2_type_short, l2_value, l3_type, l3_type_short, l3_value, address, update_date, region_id, region_code, region_name, region_te_id, region_typename, area_te_id, area_id, city_id, place_id, plan_id, street_id) VALUES (49879, '2020-07-21 23:36:46.288053', '8', 97494431, '0200', 'Северный', 'административный округ', null, '0211', 'Левобережный', 'муниципальный округ', null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, null, '0836', 'Беломорская', '001640', 'Улица', 'ул.', '193957', 'дом', 'д.', '11', 'корпус', 'к.', '1', null, null, null, 'город Москва, улица Беломорская, дом 11, корпус 1', '2020-07-21 23:49:16.167000', 67200856, '77', 'Москва', '67186391', 'Город', '67186443', null, null, null, null, 67822129);
INSERT INTO addresses (id, create_date, aolevel, global_id, region_te_code, region_te_name, region_te_typename, region_te_typename_short, areacode_omk_te, area_te_name, area_te_typename, area_te_typename_short, areacode, area_bti_code, area_name, area_typename, area_typename_short, citycode, city_bti_code, city_name, city_typename, city_typename_short, placecode, place_bti_code, place_name, place_typename, place_typename_short, plancode, plan_bti_code, plan_name, plan_typename, plan_typename_short, streetcode, street_name, street_omk_um, street_typename, street_typename_short, street_bti_code, l1_type, l1_type_short, l1_value, l2_type, l2_type_short, l2_value, l3_type, l3_type_short, l3_value, address, update_date, region_id, region_code, region_name, region_te_id, region_typename, area_te_id, area_id, city_id, place_id, plan_id, street_id) VALUES (52, '2022-11-14 17:04:30.823000', '1', -99999007, '0400', 'Регион name', null, null, '0403', 'Сведения об округе (по ОМК ТЕ)', 'Фулл', 'Шорт', '777', '0002', 'Сведения о районе в регионе', 'Тип', 'Шорт', '000', '002', 'Город', 'фулл', 'шорт', '000', '222', 'Сведения о населенном пункте', 'Населенный пункт', 'НП', '0000', '195411', 'Сведения о планировочной структуре', 'планировочная структура', 'ПС', '7041', 'Ивантеевская улица', '020930', 'Улица', 'ул', '194863', null, null, null, null, null, null, null, null, null, 'город Москва, улица Ивантеевская', '2022-11-14 17:04:30.823000', 67200856, null, 'Москва', '672008000', null, '672008222', 672008111, 672008333, 672008444, 672008555, 672008666);
INSERT INTO addresses (id, create_date, aolevel, global_id, region_te_code, region_te_name, region_te_typename, region_te_typename_short, areacode_omk_te, area_te_name, area_te_typename, area_te_typename_short, areacode, area_bti_code, area_name, area_typename, area_typename_short, citycode, city_bti_code, city_name, city_typename, city_typename_short, placecode, place_bti_code, place_name, place_typename, place_typename_short, plancode, plan_bti_code, plan_name, plan_typename, plan_typename_short, streetcode, street_name, street_omk_um, street_typename, street_typename_short, street_bti_code, l1_type, l1_type_short, l1_value, l2_type, l2_type_short, l2_value, l3_type, l3_type_short, l3_value, address, update_date, region_id, region_code, region_name, region_te_id, region_typename, area_te_id, area_id, city_id, place_id, plan_id, street_id) VALUES (53, '2022-11-14 17:04:30.823000', '1', -100, '0400', 'Регион name', null, null, '0403', 'Сведения об округе (по ОМК ТЕ)', 'Фулл', 'Шорт', '777', '0002', 'Сведения о районе в регионе', 'Тип', 'Шорт', '000', '002', 'Город', 'фулл', 'шорт', '000', '222', 'Сведения о населенном пункте', 'Населенный пункт', 'НП', '0000', '195411', 'Сведения о планировочной структуре', 'планировочная структура', 'ПС', '7041', 'Ивантеевская улица', '020930', 'Улица', 'ул', '194863', null, null, null, null, null, null, null, null, null, 'город Москва, улица Ивантеевская', '2022-11-14 17:04:30.823000', 67200856, null, 'Москва', '672008000', null, '672008222', 672008111, 672008333, 672008444, 672008555, 672008666);

INSERT INTO mo_addresses (id, mo_id, area_type_code, order_id, reject_order_id, address_id, start_date, end_date, create_date, update_date) VALUES (26392, 10310805, 10, 4, null, 11420, null, null, '2020-07-08 02:11:13.808097', null);
INSERT INTO mo_addresses (id, mo_id, area_type_code, order_id, reject_order_id, address_id, start_date, end_date, create_date, update_date) VALUES (36525, 10310805, 10, 4, null, 12903, null, null, '2020-07-08 02:11:13.808097', null);
INSERT INTO mo_addresses (id, mo_id, area_type_code, order_id, reject_order_id, address_id, start_date, end_date, create_date, update_date) VALUES (30302, 10310805, 10, 4, null, 13940, null, null, '2020-07-08 02:11:13.808097', null);
INSERT INTO mo_addresses (id, mo_id, area_type_code, order_id, reject_order_id, address_id, start_date, end_date, create_date, update_date) VALUES (33854, 10310805, 10, 4, null, 16951, null, null, '2020-07-08 02:11:13.808097', null);
INSERT INTO mo_addresses (id, mo_id, area_type_code, order_id, reject_order_id, address_id, start_date, end_date, create_date, update_date) VALUES (39289, 10310805, 10, 4, null, 24541, null, null, '2020-07-08 02:11:13.808097', null);
INSERT INTO mo_addresses (id, mo_id, area_type_code, order_id, reject_order_id, address_id, start_date, end_date, create_date, update_date) VALUES (269736, 10310805, 10, 4, null, 49879, null, null, '2021-04-02 16:03:16.108403', null);
INSERT INTO mo_addresses (id, mo_id, area_type_code, order_id, reject_order_id, address_id, start_date, end_date, create_date, update_date) VALUES (269737, 10310805, 10, 4, null, 53, null, null, '2021-04-02 16:03:16.108403', null);

INSERT INTO area_addresses (id, mo_address_id, area_id, address_id, start_date, end_date, create_date, update_date) VALUES (57117, 26392, 127, 11420, null, null, '2020-07-08 02:11:56.927054', null);
INSERT INTO area_addresses (id, mo_address_id, area_id, address_id, start_date, end_date, create_date, update_date) VALUES (57118, 30302, 127, 13940, null, null, '2020-07-08 02:11:56.927054', null);
INSERT INTO area_addresses (id, mo_address_id, area_id, address_id, start_date, end_date, create_date, update_date) VALUES (57119, 33854, 127, 16951, null, null, '2020-07-08 02:11:56.927054', null);
INSERT INTO area_addresses (id, mo_address_id, area_id, address_id, start_date, end_date, create_date, update_date) VALUES (57120, 36525, 127, 12903, null, null, '2020-07-08 02:11:56.927054', null);
INSERT INTO area_addresses (id, mo_address_id, area_id, address_id, start_date, end_date, create_date, update_date) VALUES (57121, 39289, 127, 24541, null, null, '2020-07-08 02:11:56.927054', null);
INSERT INTO area_addresses (id, mo_address_id, area_id, address_id, start_date, end_date, create_date, update_date) VALUES (206026, 269736, 127, 49879, '2021-06-21 00:00:00.000000', null, '2021-06-21 17:40:13.140000', '2021-06-21 17:40:13.140000');
INSERT INTO area_addresses (id, mo_address_id, area_id, address_id, start_date, end_date, create_date, update_date) VALUES (206027, 269737, 127, 53, '2021-06-21 00:00:00.000000', null, '2021-06-21 17:40:13.140000', '2021-06-21 17:40:13.140000');