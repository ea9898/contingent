package moscow.ptnl.contingent.ws.v5;

import moscow.ptnl.contingent.ws.BaseService;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.gov.emias2.contingent.v3._public.common.ContingentFault;
import ru.gov.emias2.contingent.v3.area.ArchiveAreaRequest;
import ru.gov.emias2.contingent.v3.area.ArchiveAreaResponse;
import ru.gov.emias2.contingent.v3.area.ArchiveAreaTypeRequest;
import ru.gov.emias2.contingent.v3.area.ArchiveAreaTypeResponse;
import ru.gov.emias2.contingent.v3.area.ArchiveLpuAreaTypeRequest;
import ru.gov.emias2.contingent.v3.area.ArchiveLpuAreaTypeResponse;
import ru.gov.emias2.contingent.v3.area.CreateArea2Request;
import ru.gov.emias2.contingent.v3.area.CreateArea2Response;
import ru.gov.emias2.contingent.v3.area.CreateAreaRequest;
import ru.gov.emias2.contingent.v3.area.CreateAreaResponse;
import ru.gov.emias2.contingent.v3.area.CreateBuildingAttachmentsRequest;
import ru.gov.emias2.contingent.v3.area.CreateBuildingAttachmentsResponse;
import ru.gov.emias2.contingent.v3.area.CreateLpuAreaTypeRequest;
import ru.gov.emias2.contingent.v3.area.CreateLpuAreaTypeResponse;
import ru.gov.emias2.contingent.v3.area.GetBuildingAttachmentsRequest;
import ru.gov.emias2.contingent.v3.area.GetBuildingAttachmentsResponse;
import ru.gov.emias2.contingent.v3.area.GetLpuAreaTypesRequest;
import ru.gov.emias2.contingent.v3.area.RemoveBuildingAttachmentsRequest;
import ru.gov.emias2.contingent.v3.area.RemoveBuildingAttachmentsResponse;
import ru.gov.emias2.contingent.v3.area.RestoreAreaRequest;
import ru.gov.emias2.contingent.v3.area.RestoreAreaResponse;
import ru.gov.emias2.contingent.v3.area.RestoreAreaTypeRequest;
import ru.gov.emias2.contingent.v3.area.RestoreAreaTypeResponse;
import ru.gov.emias2.contingent.v3.area.SearchBuildingAttachmentsRequest;
import ru.gov.emias2.contingent.v3.area.SearchBuildingAttachmentsResponse;
import ru.gov.emias2.contingent.v3.area.UpdateArea2Request;
import ru.gov.emias2.contingent.v3.area.UpdateArea2Response;
import ru.gov.emias2.contingent.v3.area.UpdateAreaRequest;
import ru.gov.emias2.contingent.v3.area.UpdateAreaResponse;
import ru.gov.emias2.contingent.v3.area.UpdateAreaTypeRequest;
import ru.gov.emias2.contingent.v3.area.UpdateAreaTypeResponse;
import ru.gov.emias2.contingent.v5._public.area.AreaPT;
import ru.gov.emias2.contingent.v5._public.area.GetAddressAreaByLpuRequest;
import ru.gov.emias2.contingent.v5._public.area.GetAddressAreaByLpuResponse;
import ru.gov.emias2.contingent.v5._public.area.GetAddressByLpuRequest;
import ru.gov.emias2.contingent.v5._public.area.GetAddressByLpuResponse;
import ru.gov.emias2.contingent.v5._public.area.GetAgeGroupsRequest;
import ru.gov.emias2.contingent.v5._public.area.GetAgeGroupsResponse;
import ru.gov.emias2.contingent.v5._public.area.GetAreaByIdRequest;
import ru.gov.emias2.contingent.v5._public.area.GetAreaByIdResponse;
import ru.gov.emias2.contingent.v5._public.area.GetAreaDetailsRequest;
import ru.gov.emias2.contingent.v5._public.area.GetAreaDetailsResponse;
import ru.gov.emias2.contingent.v5._public.area.GetAreaTypesRequest;
import ru.gov.emias2.contingent.v5._public.area.GetAreaTypesResponse;
import ru.gov.emias2.contingent.v5._public.area.GetBuildingRegistryRequest;
import ru.gov.emias2.contingent.v5._public.area.GetBuildingRegistryResponse;
import ru.gov.emias2.contingent.v5._public.area.GetEmployeesRequest;
import ru.gov.emias2.contingent.v5._public.area.GetEmployeesResponse;
import ru.gov.emias2.contingent.v5._public.area.GetLpuAreaTypesResponse;
import ru.gov.emias2.contingent.v5._public.area.GetLpuByAddressRequest;
import ru.gov.emias2.contingent.v5._public.area.GetLpuByAddressResponse;
import ru.gov.emias2.contingent.v5._public.area.GetLpusRequest;
import ru.gov.emias2.contingent.v5._public.area.GetLpusResponse;
import ru.gov.emias2.contingent.v5._public.area.GetOmkTesRequest;
import ru.gov.emias2.contingent.v5._public.area.GetOmkTesResponse;
import ru.gov.emias2.contingent.v5._public.area.GetOmkUmsRequest;
import ru.gov.emias2.contingent.v5._public.area.GetOmkUmsResponse;
import ru.gov.emias2.contingent.v5._public.area.SearchAreaRequest;
import ru.gov.emias2.contingent.v5._public.area.SearchAreaResponse;


/**
 *
 * @author m.kachalov
 */
@Service(AreaServiceImpl.SERVICE_NAME) 
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaServiceImpl extends BaseService implements AreaPT {
    
    public static final String SERVICE_NAME = "V5";
    
    public AreaServiceImpl(){}

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetLpuByAddressResponse getLpuByAddress(GetLpuByAddressRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public CreateArea2Response createArea2(CreateArea2Request body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public RemoveBuildingAttachmentsResponse removeBuildingAttachments(RemoveBuildingAttachmentsRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public SearchAreaResponse searchArea(SearchAreaRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public ArchiveLpuAreaTypeResponse archiveLpuAreaType(ArchiveLpuAreaTypeRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetAddressAreaByLpuResponse getAddressAreaByLpu(GetAddressAreaByLpuRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public UpdateArea2Response updateArea2(UpdateArea2Request body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public CreateLpuAreaTypeResponse createLpuAreaType(CreateLpuAreaTypeRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetAgeGroupsResponse getAgeGroups(GetAgeGroupsRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public UpdateAreaTypeResponse updateAreaType(UpdateAreaTypeRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public RestoreAreaResponse restoreArea(RestoreAreaRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetLpusResponse getLpus(GetLpusRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public UpdateAreaResponse updateArea(UpdateAreaRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public CreateBuildingAttachmentsResponse createBuildingAttachments(CreateBuildingAttachmentsRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetAddressByLpuResponse getAddressByLpu(GetAddressByLpuRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetAreaTypesResponse getAreaTypes(GetAreaTypesRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public RestoreAreaTypeResponse restoreAreaType(RestoreAreaTypeRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetLpuAreaTypesResponse getLpuAreaTypes(GetLpuAreaTypesRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetEmployeesResponse getEmployees(GetEmployeesRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public ArchiveAreaResponse archiveArea(ArchiveAreaRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetAreaDetailsResponse getAreaDetails(GetAreaDetailsRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetAreaByIdResponse getAreaById(GetAreaByIdRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetOmkUmsResponse getOmkUms(GetOmkUmsRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetBuildingRegistryResponse getBuildingRegistry(GetBuildingRegistryRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public CreateAreaResponse createArea(CreateAreaRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public ArchiveAreaTypeResponse archiveAreaType(ArchiveAreaTypeRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetOmkTesResponse getOmkTes(GetOmkTesRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetBuildingAttachmentsResponse getBuildingAttachments(GetBuildingAttachmentsRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public SearchBuildingAttachmentsResponse searchBuildingAttachments(SearchBuildingAttachmentsRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    
    
}
