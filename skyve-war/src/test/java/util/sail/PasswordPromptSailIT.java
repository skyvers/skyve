package util.sail;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import jakarta.annotation.Nonnull;

/**
 * Tests that real typing still reaches a page a few seconds after a sign in whose password is the user's name
 * ({@code setup}/{@code setup}, a pair Chrome treats as leaked; {@code admin}/{@code admin} did not trigger it in a
 * test of this). With Chrome's password leak detection on,
 * its tab-modal "Change your password" prompt drops every key and mouse event to the tab about a second after the sign
 * in, while scripts carry on, so the harness's later steps fail silently. See {@link CrossBrowserSelenium#passwordPromptPreferences()}.
 * The page comes from a JDK {@link HttpServer} with a plain sign in form, so this needs no Skyve server; it needs
 * Chrome, and its network access to Chrome's leak check. It uses a headed Chrome, as the prompt is browser UI.
 */
@Tag("sail")
class PasswordPromptSailIT {
	private static final String LOGIN = """
			<html><body><form method="post" action="/login">
			<input type="text" name="username" id="username" autocomplete="username">
			<input type="password" name="password" id="password" autocomplete="current-password">
			<input type="submit" id="submit" value="Sign in">
			</form></body></html>""";
	private static final String HOME = "<html><body><input type=\"text\" id=\"field\"></body></html>";

	@Nonnull private final PrimeFacesSelenide selenium = new PrimeFacesSelenide();
	private HttpServer server;

	@BeforeEach
	void startServer() throws IOException {
		server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
		server.createContext("/", PasswordPromptSailIT::login);
		server.createContext("/home", exchange -> respond(exchange, 200, HOME));
		server.start();
	}

	@AfterEach
	void cleanUp() {
		selenium.stopBrowser();
		SailServerLock.releaseAll();
		server.stop(0);
	}

	private static void login(HttpExchange exchange) throws IOException {
		if ("POST".equals(exchange.getRequestMethod())) {
			exchange.getResponseHeaders().add("Location", "/home");
			respond(exchange, 302, "");
		}
		else {
			respond(exchange, 200, LOGIN);
		}
	}

	private static void respond(HttpExchange exchange, int status, String html) throws IOException {
		byte[] body = html.getBytes(StandardCharsets.UTF_8);
		exchange.getResponseHeaders().add("Content-Type", "text/html; charset=utf-8");
		exchange.sendResponseHeaders(status, (body.length == 0) ? -1 : body.length);
		if (body.length > 0) {
			try (OutputStream out = exchange.getResponseBody()) {
				out.write(body);
			}
		}
	}

	@Test
	@SuppressWarnings("java:S2925") // nothing to poll for: the prompt is browser UI that the page cannot see, and its absence is what is tested
	void typingReachesAPageAfterASignInWithTheNameAsThePassword() throws InterruptedException {
		String url = "http://localhost:" + server.getAddress().getPort() + '/';
		selenium.startBrowser(new BrowserConfiguration().browser(BrowserConfiguration.Browsers.chrome).baseUrl(url).fixed().headless(false));
		selenium.driver.get(url);
		selenium.driver.findElement(By.id("username")).sendKeys("setup");
		selenium.driver.findElement(By.id("password")).sendKeys("setup");
		selenium.driver.findElement(By.id("submit")).click();
		Thread.sleep(3000); // the leak check and its prompt come about a second after the sign in
		WebElement field = selenium.driver.findElement(By.id("field"));
		field.click();
		field.sendKeys("typed");
		assertEquals("typed", field.getDomProperty("value"), "real keys reach the page");
	}
}
