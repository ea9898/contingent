package moscow.ptnl.contingent.nsi.endpoint;

import moscow.ptnl.contingent.domain.area.entity.Addresses;
import moscow.ptnl.contingent.nsi.domain.NsiFormTablesEnum;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementCRUDRepository;
import moscow.ptnl.contingent.nsi.domain.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.nsi.transform.NsiFormResponseMapper;
import moscow.ptnl.contingent.repository.area.AddressesCRUDRepository;
import moscow.ptnl.contingent.domain.area.repository.AddressesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.w3c.dom.Document;

import java.time.LocalDateTime;

@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
public class NsiFormProcessor {

    @Autowired
    private AddressFormingElementRepository addressFormingElementRepository;

    @Autowired
    private AddressFormingElementCRUDRepository addressFormingElementCRUDRepository;

    @Autowired
    private AddressesCRUDRepository addressesCRUDRepository;

    @Autowired
    private AddressesRepository addressesRepository;

    @Autowired
    private NsiFormResponseMapper nsiFormResponseMapper;

    public void process(Long globalId, NsiFormTablesEnum entityType, Document response) throws IllegalAccessException {
        boolean notFound;
        Object entity;

        if (NsiFormTablesEnum.ADDRESSES.equals(entityType)) {
            entity = addressesRepository.findAddressByGlobalId(globalId);

            if (notFound = (entity == null)) {
                entity = new Addresses();
                ((Addresses) entity).setGlobalId(globalId);
            }
            ((Addresses) entity).setUpdateDate(LocalDateTime.now());
        }
        else if (NsiFormTablesEnum.NSI_ADDRESS_FORMING_ELEMENT.equals(entityType)) {
            entity = addressFormingElementRepository.findAfeByGlobalId(globalId);

            if (notFound = (entity == null)) {
                entity = new NsiAddressFormingElement();
                ((NsiAddressFormingElement) entity).setGlobalId(globalId);
            }
            ((NsiAddressFormingElement) entity).setUpdateDate(LocalDateTime.now());
        }
        else {
            return;
        }
        if (!notFound) {
            //Перед непосредственным обновлением осуществляется очистка всех полей целевой таблицы в NULL (кроме global_id)
            nsiFormResponseMapper.cleanEntityFields(entity);
        }
        nsiFormResponseMapper.transformAndMergeEntity(response, entity);

        if (notFound) {
            if (NsiFormTablesEnum.ADDRESSES.equals(entityType)) {
                addressesCRUDRepository.save((Addresses) entity);
            }
            else if (NsiFormTablesEnum.NSI_ADDRESS_FORMING_ELEMENT.equals(entityType)) {
                addressFormingElementCRUDRepository.save((NsiAddressFormingElement) entity);
            }
        }
    }
}
