package org.skyve.impl.content.rest;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.MimeType;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.util.UtilImpl;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

@SuppressWarnings("static-method")
class RestRemoteContentManagerClientTest {
	private final String previousServerUrl = UtilImpl.CONTENT_REST_SERVER_URL;
	private static HttpServer server;

	@AfterEach
	void tearDown() {
		UtilImpl.CONTENT_REST_SERVER_URL = previousServerUrl;
	}

	@AfterAll
	static void stopServer() {
		if (server != null) {
			server.stop(0);
			server = null;
		}
	}

	@Test
	void startupRequiresServerUrl() throws Exception {
		UtilImpl.CONTENT_REST_SERVER_URL = null;
		try (RestRemoteContentManagerClient client = new RestRemoteContentManagerClient()) {
			assertThrows(IllegalStateException.class, client::startup);
		}
	}

	@Test
	void startupCloseAndShutdownAreNoOpsWhenConfigured() throws Exception {
		UtilImpl.CONTENT_REST_SERVER_URL = "http://localhost:8080/skyve";
		try (RestRemoteContentManagerClient client = new RestRemoteContentManagerClient()) {
			assertDoesNotThrow(client::startup);
			client.close();
			assertDoesNotThrow(client::shutdown);
		}
	}

	@Test
	void unsupportedRepositoryOperationsThrow() throws Exception {
		try (RestRemoteContentManagerClient client = new RestRemoteContentManagerClient()) {
			assertThrows(UnsupportedOperationException.class, () -> client.google("abc", 10));
			assertThrows(UnsupportedOperationException.class, client::dropIndexing);
			assertThrows(UnsupportedOperationException.class, () -> client.truncateIndexing("customer"));
			assertThrows(UnsupportedOperationException.class, () -> client.truncateAttachmentIndexing("customer"));
			assertThrows(UnsupportedOperationException.class, () -> client.truncateBeanIndexing("customer"));
			assertThrows(UnsupportedOperationException.class, client::all);
			assertThrows(UnsupportedOperationException.class, () -> client.reindex(null, true));
		}
	}

	@Test
	void contentMethodsCallConfiguredServer() throws Exception {
		List<String> requests = new ArrayList<>();
		AttachmentContent returned = attachment("B1", "avatar").attachment("avatar.txt", MimeType.plain, "remote".getBytes(StandardCharsets.UTF_8));
		startServer(exchange -> {
			String pathAndQuery = exchange.getRequestURI().toString();
			String body;
			try (java.io.InputStream requestBody = exchange.getRequestBody()) {
				body = new String(requestBody.readAllBytes(), StandardCharsets.UTF_8);
			}
			requests.add(exchange.getRequestMethod() + " " + pathAndQuery + " body=" + (! body.isEmpty()));
			if ("GET".equals(exchange.getRequestMethod())) {
				send(exchange, 200, StateUtil.encode64(returned));
			}
			else if (pathAndQuery.contains(RestRemoteContentManagerServer.ATTACHMENT_PATH) && "PUT".equals(exchange.getRequestMethod())) {
				send(exchange, 200, "content-1");
			}
			else {
				send(exchange, 200, "");
			}
		});
		try (RestRemoteContentManagerClient client = new RestRemoteContentManagerClient()) {
			BeanContent beanContent = new BeanContent(bean("B1"));
			AttachmentContent attachment = attachment("B1", "avatar").attachment("avatar.txt", MimeType.plain, "hello".getBytes(StandardCharsets.UTF_8));

			client.put(beanContent);
			client.put(attachment, true);
			client.update(attachment);
			AttachmentContent actual = client.getAttachment("content-1");
			client.removeBean("B1");
			client.removeAttachment("content-1");

			assertEquals("content-1", attachment.getContentId());
			assertEquals("avatar.txt", actual.getFileName());
		}
		assertEquals(List.of(
				"PUT /rest/content/bean body=true",
				"PUT /rest/content/attachment?index=true body=true",
				"POST /rest/content/attachment body=true",
				"GET /rest/content/attachment/content-1 body=false",
				"DELETE /rest/content/bean/B1 body=false",
				"DELETE /rest/content/attachment/content-1 body=false"), requests);
	}

	@Test
	void contentServerErrorsAreWrappedAsDomainExceptions() throws Exception {
		startServer(exchange -> send(exchange, 500, "nope"));
		try (RestRemoteContentManagerClient client = new RestRemoteContentManagerClient()) {
			assertThrows(DomainException.class, () -> client.removeBean("B1"));
		}
	}

	private static PersistentBean bean(String bizId) {
		PersistentBean bean = mock(PersistentBean.class);
		when(bean.getBizCustomer()).thenReturn("demo");
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");
		when(bean.getBizDataGroupId()).thenReturn("group1");
		when(bean.getBizUserId()).thenReturn("user1");
		when(bean.getBizId()).thenReturn(bizId);
		return bean;
	}

	private static AttachmentContent attachment(String bizId, String attributeName) {
		return new AttachmentContent("demo", "admin", "User", "group1", "user1", bizId, attributeName);
	}

	private static void startServer(ExchangeHandler handler) throws IOException {
		stopServer();
		server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
		server.createContext("/rest/content", handler::handle);
		server.start();
		UtilImpl.CONTENT_REST_SERVER_URL = "http://127.0.0.1:" + server.getAddress().getPort();
	}

	private static void send(HttpExchange exchange, int status, String body) throws IOException {
		byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
		exchange.sendResponseHeaders(status, bytes.length);
		try (java.io.OutputStream responseBody = exchange.getResponseBody()) {
			responseBody.write(bytes);
		}
		exchange.close();
	}

	@FunctionalInterface
	private interface ExchangeHandler {
		void handle(HttpExchange exchange) throws IOException;
	}
}
