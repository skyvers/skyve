package org.skyve.impl.web.filter.rest;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URI;
import java.util.Base64;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import jakarta.servlet.http.HttpServletResponse;

@Ignore
public class BasicAuthIT {
	@Test
	@SuppressWarnings("static-method")
	public void testNoBasicAuthHeader()
	throws Exception {
		HttpURLConnection c = null;
		try {
			c = (HttpURLConnection) URI.create("http://localhost:8080/skyve/rest/json/admin/Contact").toURL().openConnection();
			c.setRequestMethod("GET");
			c.setRequestProperty("Accept", "application/json");
			Assert.assertEquals(HttpServletResponse.SC_UNAUTHORIZED, c.getResponseCode());
		}
		finally {
			if (c != null) {
				c.disconnect();
			}
		}
	}
	
	@Test
	@SuppressWarnings("static-method")
	public void testInvalidCredentialsInBasicAuthHeader()
	throws Exception {
		HttpURLConnection c = null;
		try {
			c = (HttpURLConnection) URI.create("http://localhost:8080/skyve/rest/json/admin/Contact").toURL().openConnection();
			c.setRequestMethod("GET");
			c.setRequestProperty("Accept", "application/json");
			
			c.setRequestProperty("Authorization", "Basic: " + new String(Base64.getEncoder().encode("poo/bum:wee".getBytes())));
			System.out.println("Response code from server = " + c.getResponseCode());
			Assert.assertEquals(HttpServletResponse.SC_FORBIDDEN, c.getResponseCode());
		}
		finally {
			if (c != null) {
				c.disconnect();
			}
		}
	}

	@Test
	@SuppressWarnings("static-method")
	public void testBasicAuthHeader()
	throws Exception {
		HttpURLConnection c = null;
		try {
			c = (HttpURLConnection) URI.create("http://localhost:8080/skyve/rest/json/admin/Contact").toURL().openConnection();
			c.setRequestMethod("GET");
			c.setRequestProperty("Accept", "application/json");
			
			c.setRequestProperty("Authorization", "Basic: " + new String(Base64.getEncoder().encode("demo/admin:system01".getBytes())));
			System.out.println("Response code from server = " + c.getResponseCode());
			Assert.assertEquals(HttpServletResponse.SC_OK, c.getResponseCode());
			try (BufferedReader br = new BufferedReader(new InputStreamReader(c.getInputStream()))) {
				System.out.println("Output from Server ....");
				String output;
				while ((output = br.readLine()) != null) {
					System.out.println(output);
				}
			}
		}
		finally {
			if (c != null) {
				c.disconnect();
			}
		}
	}
}
