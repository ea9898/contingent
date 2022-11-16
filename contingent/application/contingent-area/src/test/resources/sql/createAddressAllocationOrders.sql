create table address_allocation_orders
(
    id          bigint               not null
        constraint address_allocation_orders_pkey
            primary key,
    number      varchar(255)         not null,
    name        varchar(255),
    date        timestamp(6),
    ouz         varchar(300),
    create_date timestamp(6)         not null,
    update_date timestamp(6),
    archived    numeric(1) default 0 not null
);