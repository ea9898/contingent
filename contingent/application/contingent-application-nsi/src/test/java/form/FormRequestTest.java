/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package form;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import moscow.ptnl.contingent.nsi.configuration.NsiFormServiceConfiguration;
import moscow.ptnl.contingent.nsi.service.NsiFormServiceHelper;
import static org.junit.jupiter.api.Assertions.assertTrue;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import ru.mos.emias.system.v1.usercontext.UserContext;

/**
 *
 * @author mkachalov
 */
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = {NsiFormServiceConfiguration.class, MockConfiguration.class})
public class FormRequestTest {
    
    private static final int THREADS_CNT = 100;
    
    @Autowired
    private NsiFormServiceHelper helper;
    
    //@Test 
    public void multiRequestsTest() throws InterruptedException {
                
        CountDownLatch latch = new CountDownLatch(THREADS_CNT);
        List<Task> tasks = new ArrayList<>();
        for (int i = 0; i < THREADS_CNT; i++) {
            Task task = new Task(){
                @Override
                public void run() {
                    super.run();
                    latch.countDown();
                }
            };
            tasks.add(task);
            new Thread(task).start();            
        }
        
        latch.await();
        
        for (Task task : tasks) {
            assertTrue(task.isSuccess());
        }
    }
    
    
    class Task implements Runnable {
        
        private boolean success = false;

        @Override
        public void run() {
            try {
                requestForNSI();
                success = true;
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        
        public boolean isSuccess() {
            return success;
        }
        
    }
    
    
    private Object requestForNSI() throws Exception {
        long formId = 127;
        long globalId = 67186393;
        UserContext userContext = new UserContext();
        userContext.setUserName("1");
        userContext.setSystemName("1");
        userContext.setUserRoleId(1);
        userContext.setIsUserRoleSystemWide(Boolean.TRUE);
        userContext.setJobExecutionId(globalId);
        userContext.setUserRights(new UserContext.UserRights(){
            @Override
            public List<Long> getUserRightId() {
                List<Long> rights = super.getUserRightId();
                rights.add(1L);
                return rights;
            }
        });

        return helper.searchByGlobalId(formId, globalId, userContext);
    }
    
}
