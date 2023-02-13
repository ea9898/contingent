package moscow.ptnl.mod.health.servlet;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.ws.rs.core.MediaType;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;

/**
 * @author sorlov
 */
public class InfoServlet extends HttpServlet {
    
    private String serviceInfo;
    
    @Override
    public void init() {
        this.serviceInfo = getInfo();
    }
    
    @Override
    protected void doGet(final HttpServletRequest req, final HttpServletResponse resp) throws ServletException, IOException {
        resp.setStatus(HttpServletResponse.SC_OK);
        resp.setHeader("Cache-Control", "private, no-store, no-cache, must-revalidate");
        resp.setHeader("Pragma", "no-cache");
        resp.addDateHeader("Last-Modified", System.currentTimeMillis());
        resp.setContentType(MediaType.APPLICATION_JSON);
        
        resp.getWriter().print(this.serviceInfo);
    }

    private String getInfo() {
        InputStream resource = getClass().getClassLoader().getResourceAsStream("info.json");
        return new BufferedReader(new InputStreamReader(resource, StandardCharsets.UTF_8))
                .lines()
                .collect(Collectors.joining("\n"));

    }
}
