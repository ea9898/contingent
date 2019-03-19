package moscow.ptnl.contingent.ws.v1;

import moscow.ptnl.contingent.ws.BaseService;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.gov.emias2.contingent.v1._public.area.AreaPT;
import ru.gov.emias2.contingent.v1._public.area.GetAddressByLpuRequest;
import ru.gov.emias2.contingent.v1._public.area.GetAddressByLpuResponse;
import ru.gov.emias2.contingent.v1._public.area.GetAgeGroupsRequest;
import ru.gov.emias2.contingent.v1._public.area.GetAgeGroupsResponse;
import ru.gov.emias2.contingent.v1._public.area.GetAreaByIdResponse;
import ru.gov.emias2.contingent.v1._public.area.GetAreaDetailsRequest;
import ru.gov.emias2.contingent.v1._public.area.GetAreaDetailsResponse;
import ru.gov.emias2.contingent.v1._public.area.GetAreaTypesRequest;
import ru.gov.emias2.contingent.v1._public.area.GetAreaTypesResponse;
import ru.gov.emias2.contingent.v1._public.area.GetBuildingRegistryRequest;
import ru.gov.emias2.contingent.v1._public.area.GetBuildingRegistryResponse;
import ru.gov.emias2.contingent.v1._public.area.GetEmployeesRequest;
import ru.gov.emias2.contingent.v1._public.area.GetEmployeesResponse;
import ru.gov.emias2.contingent.v1._public.area.GetLpuAreaTypesResponse;
import ru.gov.emias2.contingent.v1._public.area.GetLpusRequest;
import ru.gov.emias2.contingent.v1._public.area.GetLpusResponse;
import ru.gov.emias2.contingent.v1._public.area.GetOmkTesRequest;
import ru.gov.emias2.contingent.v1._public.area.GetOmkTesResponse;
import ru.gov.emias2.contingent.v1._public.area.GetOmkUmsRequest;
import ru.gov.emias2.contingent.v1._public.area.GetOmkUmsResponse;
import ru.gov.emias2.contingent.v1._public.area.SearchAreaRequest;
import ru.gov.emias2.contingent.v1._public.area.SearchAreaResponse;
import ru.gov.emias2.contingent.v1._public.common.ContingentFault;
import ru.gov.emias2.contingent.v1.area.ArchiveAreaRequest;
import ru.gov.emias2.contingent.v1.area.ArchiveAreaResponse;
import ru.gov.emias2.contingent.v1.area.ArchiveLpuAreaTypeRequest;
import ru.gov.emias2.contingent.v1.area.ArchiveLpuAreaTypeResponse;
import ru.gov.emias2.contingent.v1.area.CreateAreaRequest;
import ru.gov.emias2.contingent.v1.area.CreateAreaResponse;
import ru.gov.emias2.contingent.v1.area.CreateLpuAreaTypeRequest;
import ru.gov.emias2.contingent.v1.area.CreateLpuAreaTypeResponse;
import ru.gov.emias2.contingent.v1.area.GetAreaByIdRequest;
import ru.gov.emias2.contingent.v1.area.GetLpuAreaTypesRequest;
import ru.gov.emias2.contingent.v1.area.RestoreAreaRequest;
import ru.gov.emias2.contingent.v1.area.RestoreAreaResponse;
import ru.gov.emias2.contingent.v1.area.UpdateAreaRequest;
import ru.gov.emias2.contingent.v1.area.UpdateAreaResponse;

/**
 *
 * @author mkachalov
 */
@Service(AreaServiceImpl.SERVICE_NAME) 
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaServiceImpl extends BaseService implements AreaPT {
    
    public static final String SERVICE_NAME = "V1";

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public UpdateAreaResponse updateArea(UpdateAreaRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetAreaTypesResponse getAreaTypes(GetAreaTypesRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetAddressByLpuResponse getAddressByLpu(GetAddressByLpuRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetLpuAreaTypesResponse getLpuAreaTypes(GetLpuAreaTypesRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public SearchAreaResponse searchArea(SearchAreaRequest body) throws ContingentFault {
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
    public GetEmployeesResponse getEmployees(GetEmployeesRequest body) throws ContingentFault {
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
    public ArchiveLpuAreaTypeResponse archiveLpuAreaType(ArchiveLpuAreaTypeRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetBuildingRegistryResponse getBuildingRegistry(GetBuildingRegistryRequest body) throws ContingentFault {
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
    public CreateAreaResponse createArea(CreateAreaRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetLpusResponse getLpus(GetLpusRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public RestoreAreaResponse restoreArea(RestoreAreaRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetOmkTesResponse getOmkTes(GetOmkTesRequest body) throws ContingentFault {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
