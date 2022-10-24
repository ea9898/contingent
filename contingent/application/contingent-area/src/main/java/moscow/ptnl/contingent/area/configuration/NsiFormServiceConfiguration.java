package moscow.ptnl.contingent.area.configuration;

import moscow.ptnl.contingent.area.configuration.security.SecurityHeaderGenerator;
import org.apache.cxf.binding.BindingConfiguration;
import org.apache.cxf.jaxws.JaxWsProxyFactoryBean;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import ru.mos.emias.formproduct.formservice.v1.FormServicePortType;

import javax.xml.ws.soap.SOAPBinding;

@Configuration
@PropertySource("classpath:application.properties")
public class NsiFormServiceConfiguration {

    @Bean
    public FormServicePortType formServiceFactoryBean(@Value("${nsi.form.service.address}") String address,
                                                      @Value("${security.upk2.login}") String login,
                                                      @Value("${nsi.service.username}") String userName) {

        JaxWsProxyFactoryBean jaxWsProxyFactoryBean = new JaxWsProxyFactoryBean();
        jaxWsProxyFactoryBean.setServiceClass(FormServicePortType.class);
        jaxWsProxyFactoryBean.setAddress(address);
        jaxWsProxyFactoryBean.setBindingConfig(new BindingConfiguration() {
            @Override
            public String getBindingId() {
                return SOAPBinding.SOAP12HTTP_BINDING;
            }
        });

        jaxWsProxyFactoryBean.getOutInterceptors().add(new SecurityHeaderGenerator(login, userName));

//        interceptorService.setupInterceptors(jaxWsProxyFactoryBean);

        return (FormServicePortType) jaxWsProxyFactoryBean.create();
    }
}
