package org.skyve.impl.geoip;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.ArgumentMatchers.any;

import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;
import java.io.IOException;
import java.util.Objects;

import org.junit.jupiter.api.Test;
import org.skyve.util.IPGeolocation;

class IPInfoIoTest {
	@Test
	@SuppressWarnings("static-method")
	void doGeolocationReturnsEmptyAndReinterruptsOnInterruptedException() {
		IPInfoIo service = new IPInfoIo() {
			@Override
			protected HttpResponse<String> send(HttpClient client, HttpRequest request) throws IOException, InterruptedException {
				throw new InterruptedException("interrupted");
			}
		};

		try {
			Thread.interrupted();
			IPGeolocation geolocation = service.doGeolocation("203.0.113.10");
			assertSame(IPGeolocation.EMPTY, geolocation);
			assertTrue(Thread.currentThread().isInterrupted());
		}
		finally {
			Thread.interrupted();
		}
	}

	@Test
	@SuppressWarnings("static-method")
	void doGeolocationReturnsEmptyOnSendFailure() {
		IPInfoIo service = new IPInfoIo() {
			@Override
			protected HttpResponse<String> send(HttpClient client, HttpRequest request) throws IOException, InterruptedException {
				throw new IllegalStateException("boom");
			}
		};

		IPGeolocation geolocation = service.doGeolocation("203.0.113.11");
		assertSame(IPGeolocation.EMPTY, geolocation);
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void doGeolocationParsesSuccessfulPayloadWithLocation() {
		final HttpResponse<String> response = mock(HttpResponse.class);
		when(response.statusCode()).thenReturn(Integer.valueOf(200));
		when(response.body()).thenReturn("{\"city\":\"Adelaide\",\"region\":\"SA\",\"country\":\"AU\",\"loc\":\"-34.9285,138.6007\"}");

		IPInfoIo service = new IPInfoIo() {
			@Override
			protected HttpResponse<String> send(HttpClient client, HttpRequest request) throws IOException, InterruptedException {
				return response;
			}
		};

		IPGeolocation geolocation = service.doGeolocation("203.0.113.12");
		assertEquals("Adelaide", geolocation.city());
		assertEquals("SA", geolocation.region());
		assertEquals("AU", geolocation.countryCode());
		assertNotNull(geolocation.location());
		var location = Objects.requireNonNull(geolocation.location());
		assertEquals(Double.valueOf(138.6007d), Double.valueOf(location.getX()));
		assertEquals(Double.valueOf(-34.9285d), Double.valueOf(location.getY()));
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void doGeolocationReturnsEmptyOnNon200Response() {
		final HttpResponse<String> response = mock(HttpResponse.class);
		when(response.statusCode()).thenReturn(Integer.valueOf(503));
		when(response.body()).thenReturn("{}");

		IPInfoIo service = new IPInfoIo() {
			@Override
			protected HttpResponse<String> send(HttpClient client, HttpRequest request) throws IOException, InterruptedException {
				return response;
			}
		};

		IPGeolocation geolocation = service.doGeolocation("203.0.113.13");
		assertSame(IPGeolocation.EMPTY, geolocation);
		assertNull(geolocation.location());
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void doGeolocationParsesSuccessfulPayloadWithoutLoc() {
		final HttpResponse<String> response = mock(HttpResponse.class);
		when(response.statusCode()).thenReturn(Integer.valueOf(200));
		when(response.body()).thenReturn("{\"city\":\"Adelaide\",\"region\":\"SA\",\"country\":\"AU\"}");

		IPInfoIo service = new IPInfoIo() {
			@Override
			protected HttpResponse<String> send(HttpClient client, HttpRequest request) throws IOException, InterruptedException {
				return response;
			}
		};

		IPGeolocation geolocation = service.doGeolocation("203.0.113.14");
		assertEquals("Adelaide", geolocation.city());
		assertEquals("SA", geolocation.region());
		assertEquals("AU", geolocation.countryCode());
		assertNull(geolocation.location());
	}

	@Test
	@SuppressWarnings({ "static-method", "boxing" })
	void doGeolocationParsesSuccessfulPayloadWithInvalidLocFormat() {
		final HttpResponse<String> response = mock(HttpResponse.class);
		when(response.statusCode()).thenReturn(Integer.valueOf(200));
		when(response.body()).thenReturn("{\"city\":\"Adelaide\",\"region\":\"SA\",\"country\":\"AU\",\"loc\":\"-34.9285\"}");

		IPInfoIo service = new IPInfoIo() {
			@Override
			protected HttpResponse<String> send(HttpClient client, HttpRequest request) throws IOException, InterruptedException {
				return response;
			}
		};

		IPGeolocation geolocation = service.doGeolocation("203.0.113.15");
		assertEquals("Adelaide", geolocation.city());
		assertEquals("SA", geolocation.region());
		assertEquals("AU", geolocation.countryCode());
		assertNull(geolocation.location());
	}

	@Test
	@SuppressWarnings({ "static-method", "unchecked" })
	void sendDelegatesToHttpClient() throws Exception {
		HttpClient client = mock(HttpClient.class);
		HttpRequest request = HttpRequest.newBuilder().uri(URI.create("https://example.com")).GET().build();
		HttpResponse<String> expected = mock(HttpResponse.class);
		when(client.send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class))).thenReturn(expected);

		IPInfoIo service = new IPInfoIo();
		HttpResponse<String> actual = service.send(client, request);

		assertSame(expected, actual);
		verify(client).send(any(HttpRequest.class), any(HttpResponse.BodyHandler.class));
	}
}
