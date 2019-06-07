/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.endpoint;

import moscow.ptnl.contingent.domain.esu.EsuOutput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import static moscow.ptnl.contingent.configuration.EventChannelsConfiguration.ESU_EVENT_CHANNEL_NAME;

/**
 *
 * @author m.kachalov
 */
@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
@MessageEndpoint
public class ESUEventEndpoint {

    private static final Logger LOG = LoggerFactory.getLogger(ESUEventEndpoint.class);

    @ServiceActivator(inputChannel = ESU_EVENT_CHANNEL_NAME)
    public void esuOutputConsumer(Message<EsuOutput> msg) {
        LOG.debug("input message: {}", msg.getPayload().getEsuId());

    }
}
