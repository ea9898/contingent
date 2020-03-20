package moscow.ptnl.contingent.nsi.configuration;

import moscow.ptnl.contingent.nsi.configuration.security.SecurityHeaderGenerator;
import org.apache.cxf.binding.BindingConfiguration;
import org.apache.cxf.jaxws.JaxWsProxyFactoryBean;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import ru.mos.emias.nsiproduct.nsiserviceasyncfasad.v1.NsiServiceAsyncFasadPortType;

import javax.xml.ws.soap.SOAPBinding;

@Configuration
public class NsiServiceConfiguration {

    @Bean(name = "nsiServiceAsyncFasadPortType")
    public NsiServiceAsyncFasadPortType nsiServiceAsyncFasadPortType(@Value("${nsi.ehd.service.address}") String address,
                                                                     @Value("${security.upk2.login}") String upkLogin,
                                                                     @Value("${nsi.service.username}") String nsiUserName) {
        JaxWsProxyFactoryBean jaxWsProxyFactoryBean = new JaxWsProxyFactoryBean();
        jaxWsProxyFactoryBean.setServiceClass(NsiServiceAsyncFasadPortType.class);
        jaxWsProxyFactoryBean.setAddress(address);

        jaxWsProxyFactoryBean.setBindingConfig(new BindingConfiguration() {
            @Override
            public String getBindingId() {
                return SOAPBinding.SOAP12HTTP_BINDING;
            }
        });

        jaxWsProxyFactoryBean.getOutInterceptors().add(new SecurityHeaderGenerator(upkLogin, nsiUserName));

        return (NsiServiceAsyncFasadPortType) jaxWsProxyFactoryBean.create();
    }

}