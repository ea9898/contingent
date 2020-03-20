package moscow.ptnl.contingent.nsi.transform;

import org.springframework.stereotype.Component;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.UpdateAddressByGlobalIdResponse;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.UpdateAddressByGlobalIdResponse.UnrecognizedAddresses;

import java.util.List;

@Component
public class UpdateAddressByGlobalIdResponseMapper {

    public UpdateAddressByGlobalIdResponse transform(List<Long> unrecognizedAddresses) {
        UpdateAddressByGlobalIdResponse response = new UpdateAddressByGlobalIdResponse();

        if (unrecognizedAddresses != null && !unrecognizedAddresses.isEmpty()) {
            response.setUnrecognizedAddresses(new UnrecognizedAddresses());
            response.getUnrecognizedAddresses().getArGlobalId().addAll(unrecognizedAddresses);
        }
        return response;
    }
}
