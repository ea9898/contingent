package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.domain.nsi.entity.NsiPush;
import moscow.ptnl.contingent.domain.nsi.entity.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.Table;
import moscow.ptnl.contingent.repository.nsi.NsiPushEventCRUDRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaType;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaTypeClass;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaTypeKind;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaTypeMedicalPositions;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaTypeRelations;
import static moscow.ptnl.contingent.nsi.pushaccepter.PushAccepterMapper.mapAreaTypeSpecializations;

@Component
public class PushAccepterImpl extends PushAccepter {

    @Autowired
    @Qualifier(EventChannelsConfiguration.NSI_EVENT_CHANNEL_NAME)
    private MessageChannel nsiChannel;

    @Autowired
    NsiPushEventCRUDRepository nsiPushEventCRUDRepository;

    @Override
    public Answer getPushSpec(Table table) {
        return new Answer(true, "ok");
    }

    @Override
    public Answer getPushForm(String response) {
        return new Answer(true, "ok");
    }

    @Override
    public Answer getPush(Package pack, long savedId) {
        NsiPush nsiPush = new NsiPush(pack.catalog.data.action, savedId);
        switch (NsiTablesEnum.getByName(pack.catalog.name)) {
            case AREA_TYPE:
                nsiPush.setEntity(mapAreaType(pack));
                break;
            case AREA_TYPE_CLASS:
                nsiPush.setEntity(mapAreaTypeClass(pack));
                break;
            case AREA_TYPE_KIND:
                nsiPush.setEntity(mapAreaTypeKind(pack));
                break;
            case AREA_TYPE_MEDICAL_POSITIONS:
                nsiPush.setEntity(mapAreaTypeMedicalPositions(pack));
                break;
            case AREA_TYPE_RELATIONS:
                nsiPush.setEntity(mapAreaTypeRelations(pack));
                break;
            case AREA_TYPE_SPECIALIZATIONS:
                nsiPush.setEntity(mapAreaTypeSpecializations(pack));
        }
        nsiChannel.send(MessageBuilder.withPayload(nsiPush).build());
        return new Answer(true, "ok");
    }
}
