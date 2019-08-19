package moscow.ptnl.contingent.nsi.pushaccepter;

import moscow.ptnl.contingent.configuration.EventChannelsConfiguration;
import moscow.ptnl.contingent.domain.nsi.NsiPushEventConstraint;
import moscow.ptnl.contingent.domain.nsi.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.Table;
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

    @Override
    public Answer getPushSpec(Table table) {
        return new Answer(true, "ok");
    }

    @Override
    public Answer getPushForm(String response) {
        return new Answer(true, "ok");
    }

    @Override
    public Answer getPush(Package pack, Long pushEventId) {
        Object pushEventEntity = null;
        switch (NsiTablesEnum.getByName(pack.catalog.name)) {
            case AREA_TYPE:
                pushEventEntity = mapAreaType(pack);
                break;
            case AREA_TYPE_CLASS:
                pushEventEntity = mapAreaTypeClass(pack);
                break;
            case AREA_TYPE_KIND:
                pushEventEntity = mapAreaTypeKind(pack);
                break;
            case AREA_TYPE_MEDICAL_POSITIONS:
                pushEventEntity = mapAreaTypeMedicalPositions(pack);
                break;
            case AREA_TYPE_RELATIONS:
                pushEventEntity = mapAreaTypeRelations(pack);
                break;
            case AREA_TYPE_SPECIALIZATIONS:
                pushEventEntity = mapAreaTypeSpecializations(pack);
        }
        nsiChannel.send(MessageBuilder
                .withPayload(pushEventEntity)
                .setHeader(NsiPushEventConstraint.PUSH_EVENT_ID_HEADER, pushEventId)
                .setHeader(NsiPushEventConstraint.PUSH_EVENT_ACTION_HEADER, pack.catalog.data.action)
                .build());
        return new Answer(true, "ok");
    }
}
