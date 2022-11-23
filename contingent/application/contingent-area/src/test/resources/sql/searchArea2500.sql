INSERT INTO address_allocation_orders (id, number, name, date, ouz, create_date, update_date, archived) VALUES (1, '100', 'Это Имя', '2022-01-01 00:00:00.000000', 'Это ОУЗ', '2022-11-07 09:50:30.421000', '2022-11-07 09:50:30.421000', 0);

INSERT INTO addresses (id, create_date, aolevel, global_id, region_te_code, region_te_name, region_te_typename, region_te_typename_short, areacode_omk_te, area_te_name, area_te_typename, area_te_typename_short, areacode, area_bti_code, area_name, area_typename, area_typename_short, citycode, city_bti_code, city_name, city_typename, city_typename_short, placecode, place_bti_code, place_name, place_typename, place_typename_short, plancode, plan_bti_code, plan_name, plan_typename, plan_typename_short, streetcode, street_name, street_omk_um, street_typename, street_typename_short, street_bti_code, l1_type, l1_type_short, l1_value, l2_type, l2_type_short, l2_value, l3_type, l3_type_short, l3_value, address, update_date, region_id, region_code, region_name, region_te_id, region_typename, area_te_id, area_id, city_id, place_id, plan_id, street_id) VALUES (52, '2022-11-14 17:04:30.823000', '7', -99999007, '0400', 'Регион name', null, null, '0403', 'Сведения об округе (по ОМК ТЕ)', 'Фулл', 'Шорт', '777', '0002', 'Сведения о районе в регионе', 'Тип', 'Шорт', '000', '002', 'Город', 'фулл', 'шорт', '000', '222', 'Сведения о населенном пункте', 'Населенный пункт', 'НП', '0000', '195411', 'Сведения о планировочной структуре', 'планировочная структура', 'ПС', '7041', 'Ивантеевская улица', '020930', 'Улица', 'ул', '194863', null, null, null, null, null, null, null, null, null, 'город Москва, улица Ивантеевская', '2022-11-14 17:04:30.823000', 67200856, null, 'Москва', '672008000', null, '672008222', 672008111, 672008333, 672008444, 672008555, 672008666);

INSERT INTO mo_addresses (id, mo_id, area_type_code, order_id, reject_order_id, address_id, start_date, end_date, create_date, update_date) VALUES (32, 200001003712, 20, 1, null, 52, '2022-11-14 00:00:00.000000', null, '2022-11-14 17:04:32.131000', null);
INSERT INTO mo_addresses (id, mo_id, area_type_code, order_id, reject_order_id, address_id, start_date, end_date, create_date, update_date) VALUES (33, 200001003712, 10, 1, null, 52, '2022-11-14 00:00:00.000000', null, '2022-11-14 17:04:32.131000', null);