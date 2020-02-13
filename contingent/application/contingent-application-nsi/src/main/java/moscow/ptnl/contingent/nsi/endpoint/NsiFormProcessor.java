package moscow.ptnl.contingent.nsi.endpoint;

import moscow.ptnl.contingent.nsi.domain.NsiFormTablesEnum;
import moscow.ptnl.contingent.nsi.domain.area.NsiAddressFormingElement;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AddressFormingElementRepository;
import moscow.ptnl.contingent.nsi.transform.NsiFormResponseMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
    private NsiFormResponseMapper nsiFormResponseMapper;

    public void process(Long globalId, NsiFormTablesEnum entityType, Document response) throws IllegalAccessException {
        boolean notFound = false;
        Object entity = null;

        if (NsiFormTablesEnum.ADDRESSES.equals(entityType)) {
            entity = null; //TODO fix
//                    entity = entity == null ? new Addresses() : entity;
        }
        else if (NsiFormTablesEnum.NSI_ADDRESS_FORMING_ELEMENT.equals(entityType)) {
            entity = addressFormingElementRepository.findAfeByGlobalId(globalId);

            if (notFound = (entity == null)) {
                entity = new NsiAddressFormingElement();
                ((NsiAddressFormingElement) entity).setGlobalId(globalId);
            }
            ((NsiAddressFormingElement) entity).setUpdateDate(LocalDateTime.now());
        }
        nsiFormResponseMapper.transformAndMergeEntity(response, entity);

        if (notFound) {
            if (NsiFormTablesEnum.ADDRESSES.equals(entityType)) {

            }
            else if (NsiFormTablesEnum.NSI_ADDRESS_FORMING_ELEMENT.equals(entityType)) {
                addressFormingElementCRUDRepository.save((NsiAddressFormingElement) entity);
            }
        }
    }
}
