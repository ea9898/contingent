package moscow.ptnl.contingent.nsi.ws;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.domain.nsi.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper;
import moscow.ptnl.contingent.nsi.pushaccepter.PushAccepter;
import moscow.ptnl.contingent.repository.nsi.NsiPushEventCRUDRepository;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import ru.mos.emias.nsiproduct.nsiservice.v1.types.GetCatalogItemsRequest;
import ru.mos.emias.nsiproduct.nsiservice.v1.types.GetCatalogItemsResponse;
import ru.mos.emias.nsiproduct.nsiserviceasyncfasad.v1.NsiServiceAsyncFasadPortType;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.AdminServicePortType;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.Fault;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.SyncNsiRequest;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.SyncNsiResponse;

import java.util.List;

import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeClasses;

@Service(NsiAdminWebServiceImpl.SERVICE_NAME)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class NsiAdminWebServiceImpl implements AdminServicePortType {

    public static final String SERVICE_NAME = "NSI_ADMIN_V1";

    @Autowired
    PushAccepter pushAccepter;

    @Autowired
    NsiPushEventCRUDRepository nsiPushEventCRUDRepository;

    @Autowired
    NsiServiceAsyncFasadPortType nsiService;


    @Override
    public SyncNsiResponse syncNsi(SyncNsiRequest body) throws Fault {
        body.getCatalogCode().forEach(catalog -> {
            System.out.println(catalog);
            GetCatalogItemsRequest catalogDataRequest = new GetCatalogItemsRequest();
            catalogDataRequest.setIdCatalog(catalog);
            try {
                GetCatalogItemsResponse response = nsiService.getCatalogItems(catalogDataRequest);
                switch (NsiTablesEnum.getByCode(catalog)) {
                    case AREA_TYPE:
                        List<AreaType> areaTypes = NsiEntityMapper.mapAreaTypes(response.getEhdCatalogItems().getRows());
                        System.out.println(areaTypes);
                        break;
                    case AREA_TYPE_CLASS:
                        List<AreaTypeClass> areaTypeClasses = mapAreaTypeClasses(response.getEhdCatalogItems().getRows());
                        System.out.println(areaTypeClasses);
                        break;
/*                    case AREA_TYPE_KIND:
                        pushEventEntity = mapAreaTypeKind(pack);
                        break;
                    case AREA_TYPE_MEDICAL_POSITIONS:
                        pushEventEntity = mapAreaTypeMedicalPositions(pack);
                        break;
                    case AREA_TYPE_RELATIONS:
                        pushEventEntity = mapAreaTypeRelations(pack);
                        break;
                    case AREA_TYPE_SPECIALIZATIONS:
                        pushEventEntity = mapAreaTypeSpecializations(pack);*/
                }
            } catch (ru.mos.emias.nsiproduct.nsiserviceasyncfasad.v1.Fault fault) {
                fault.printStackTrace();
            }

        });
        return new SyncNsiResponse();
    }
}
