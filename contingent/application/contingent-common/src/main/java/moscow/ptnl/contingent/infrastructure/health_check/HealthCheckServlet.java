package moscow.ptnl.contingent.infrastructure.health_check;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.core.MediaType;
import java.io.IOException;
import java.io.Writer;
import java.time.LocalDateTime;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

/**
 * @author sorlov
 */
public class HealthCheckServlet extends HttpServlet {

    private HealthCheckService service;

    public HealthCheckService getService() {
        return service;
    }

    public void setService(HealthCheckService service) {
        this.service = service;
    }

    @Override
    protected void doGet(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
        resp.setStatus(HttpServletResponse.SC_OK);
        resp.setHeader("Cache-Control", "private, no-store, no-cache, must-revalidate");
        resp.setHeader("Pragma", "no-cache");
        resp.addDateHeader("Last-Modified", System.currentTimeMillis());
        resp.setContentType(MediaType.TEXT_PLAIN);

        String response;

        try {
            Future<String> databaseHealthChecker = null;

            try {
                databaseHealthChecker = service.databaseHealthCheck();
                response = databaseHealthChecker.get(30000, TimeUnit.MILLISECONDS);
            }
            finally {
                if (databaseHealthChecker != null) {
                    databaseHealthChecker.cancel(true);
                }
            }
            response += "\n" + service.esuHealthCheck();

            try (Writer writer = resp.getWriter()) {
                writer.write(response);
                writer.flush();
            }
        }
        catch (Exception e) {
            resp.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);

            try (Writer writer = resp.getWriter()) {
                writer.write(e.toString());
                writer.flush();
            }
        }
    }

    @Override
    protected void doPost(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
        resp.setStatus(HttpServletResponse.SC_BAD_REQUEST);
    }
}
