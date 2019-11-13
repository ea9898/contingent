package moscow.ptnl.contingent.nsi.pushaccepter;

import static moscow.ptnl.contingent.nsi.configuration.Constraint.NSI_EVENT_CHANNEL_NAME;

import moscow.ptnl.contingent.nsi.domain.NsiExternalEntity;
import moscow.ptnl.contingent.nsi.domain.NsiPushEventConstraint;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClass;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKind;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.domain.area.Gender;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom;
import moscow.ptnl.contingent.nsi.domain.area.Specialization;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsing.Package;
import moscow.ptnl.contingent.nsi.pushaccepter.xmlparsingS.Table;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.integration.support.MessageBuilder;
import org.springframework.messaging.MessageChannel;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
public class PushAccepterImpl extends PushAccepter {

    private static final String NSI_ENTITY_SOURCE = "push";

    @Autowired
    private NsiEntityMapper entityMapper;

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
        switch (NsiTablesEnum.getByCatalogName(pack.catalog.name)) {
            case AREA_TYPE:
                pushEventEntity = entityMapper.mapTyped(pack, AreaType.class); 
                break;
            case AREA_TYPE_CLASS:
                pushEventEntity = entityMapper.mapTyped(pack, AreaTypeClass.class);
                break;
            case AREA_TYPE_KIND:
                pushEventEntity = entityMapper.mapTyped(pack, AreaTypeKind.class);
                break;
            case AREA_TYPE_MEDICAL_POSITIONS:
                pushEventEntity = entityMapper.mapTyped(pack, AreaTypeMedicalPositions.class);
                break;
            case AREA_TYPE_RELATIONS:
                pushEventEntity = entityMapper.mapTyped(pack, AreaTypeRelations.class);
                break;
            case AREA_TYPE_SPECIALIZATIONS:
                pushEventEntity = entityMapper.mapTyped(pack, AreaTypeSpecializations.class);
                break;
            case SPECIALIZATION:
                pushEventEntity = entityMapper.mapTyped(pack, Specialization.class);
                break;
            case POSITION_CODE:
                pushEventEntity = entityMapper.mapTyped(pack, PositionCode.class);
                break;
            case GENDER:
                pushEventEntity = entityMapper.mapTyped(pack, Gender.class);
                break;
            case POLICY_TYPE:
                pushEventEntity = entityMapper.mapTyped(pack, PolicyType.class);
                break;
            case D_POSITION_NOM:
                pushEventEntity = entityMapper.mapTyped(pack, PositionNom.class);
                break;
        }
        if (pushEventEntity instanceof NsiExternalEntity) {
            ((NsiExternalEntity) pushEventEntity).setUpdateDate(LocalDateTime.now());
            ((NsiExternalEntity) pushEventEntity).setSource(NSI_ENTITY_SOURCE);
        }
        nsiChannel.send(MessageBuilder
                .withPayload(pushEventEntity)
                .setHeader(NsiPushEventConstraint.PUSH_EVENT_ID_HEADER, pushEventId)
                .setHeader(NsiPushEventConstraint.PUSH_EVENT_ACTION_HEADER, pack.catalog.data.action)
                .build());
        return new Answer(true, "ok");
    }
}
