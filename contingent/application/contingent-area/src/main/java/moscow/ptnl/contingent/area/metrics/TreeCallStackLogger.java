package moscow.ptnl.contingent.area.metrics;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.invoke.MethodHandles;

/**
 * Логгер кол-стека, отображающий дерево вызова в иерархическом виде.
 * 
 */
class TreeCallStackLogger {

    private final static Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    private TreeMethodCall current;
    private TreeMethodCall root;

    TreeCallStackLogger() {
        root = current = TreeMethodCall.root();
    }

    void beforeCall(String name) {
        TreeMethodCall call = TreeMethodCall.create(name, current);
        current.addNestedCall(call);
        current = call;
    }

    void afterCall(boolean isExceptionThrown) {
        current.stopWatch();
        current = current.parent;

        if (current == root) { //вышли из корневого метода, пора писать в лог
            current.stopWatch();
            StringBuilder tree = root.toLogNode().print();

            if (isExceptionThrown) {
                tree.append(System.lineSeparator()).append("With exception thrown!!!");
            }
            logger.info("{}", tree);
            root = current = null;
        }
    }

    boolean currentIsRoot() {
	    return current == root;
    }
	    
}
