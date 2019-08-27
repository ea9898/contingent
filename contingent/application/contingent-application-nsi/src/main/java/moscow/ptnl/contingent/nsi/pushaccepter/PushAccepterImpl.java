package moscow.ptnl.contingent.nsi.pushaccepter;

import static moscow.ptnl.contingent.configuration.nsi.Constraint.NSI_EVENT_CHANNEL_NAME;
import moscow.ptnl.contingent.domain.nsi.NsiPushEventConstraint;
import moscow.ptnl.contingent.domain.nsi.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.Table;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaType;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeClass;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeKind;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeMedicalPosition;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeRelation;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeSpecialization;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapGender;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapPositionCode;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapSpecialization;

@Component
public class PushAccepterImpl extends PushAccepter {

    @Autowired
    @Qualifier(NSI_EVENT_CHANNEL_NAME)
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
                pushEventEntity = mapAreaTypeMedicalPosition(pack);
                break;
            case AREA_TYPE_RELATIONS:
                pushEventEntity = mapAreaTypeRelation(pack);
                break;
            case AREA_TYPE_SPECIALIZATIONS:
                pushEventEntity = mapAreaTypeSpecialization(pack);
                break;
            case SPECIALIZATIONS:
                pushEventEntity = mapSpecialization(pack);
                break;
            case POSITION_CODE:
                pushEventEntity = mapPositionCode(pack);
                break;
            case GENDER:
                pushEventEntity = mapGender(pack);
        }
        nsiChannel.send(MessageBuilder
                .withPayload(pushEventEntity)
                .setHeader(NsiPushEventConstraint.PUSH_EVENT_ID_HEADER, pushEventId)
                .setHeader(NsiPushEventConstraint.PUSH_EVENT_ACTION_HEADER, pack.catalog.data.action)
                .build());
        return new Answer(true, "ok");
    }
}
