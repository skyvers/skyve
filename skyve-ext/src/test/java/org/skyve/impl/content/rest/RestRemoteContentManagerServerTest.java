package org.skyve.impl.content.rest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.annotation.Annotation;
import java.net.URI;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletionStage;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.BeanContent;
import org.skyve.content.MimeType;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.cache.StateUtil;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.content.NoOpContentManager;

import jakarta.ws.rs.SeBootstrap;
import jakarta.ws.rs.core.Application;
import jakarta.ws.rs.core.CacheControl;
import jakarta.ws.rs.core.EntityPart;
import jakarta.ws.rs.core.EntityTag;
import jakarta.ws.rs.core.GenericType;
import jakarta.ws.rs.core.Link;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.MultivaluedHashMap;
import jakarta.ws.rs.core.MultivaluedMap;
import jakarta.ws.rs.core.NewCookie;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.UriBuilder;
import jakarta.ws.rs.core.Variant;
import jakarta.ws.rs.ext.RuntimeDelegate;

@SuppressWarnings({"static-method", "resource"})
class RestRemoteContentManagerServerTest {
	private RuntimeDelegate originalDelegate;
	private Class<? extends AbstractContentManager> originalContentManagerClass;

	@BeforeEach
	void setUp() {
		try {
			originalDelegate = RuntimeDelegate.getInstance();
		}
		catch (@SuppressWarnings("unused") RuntimeException e) {
			originalDelegate = null;
		}
		RuntimeDelegate.setInstance(new TestRuntimeDelegate());
		originalContentManagerClass = AbstractContentManager.IMPLEMENTATION_CLASS;
		AbstractContentManager.IMPLEMENTATION_CLASS = CapturingContentManager.class;
		CapturingContentManager.reset();
	}

	@AfterEach
	void tearDown() {
		RuntimeDelegate.setInstance(originalDelegate);
		AbstractContentManager.IMPLEMENTATION_CLASS = originalContentManagerClass;
		CapturingContentManager.reset();
	}

	@Test
	void putBeanReturnsServerErrorForMalformedPayload() {
		Response response = new RestRemoteContentManagerServer().put("not-gzip-base64");

		assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus());
	}

	@Test
	void putAttachmentReturnsServerErrorForMalformedPayload() {
		Response response = new RestRemoteContentManagerServer().put("not-gzip-base64", true);

		assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus());
	}

	@Test
	void updateAttachmentReturnsServerErrorForMalformedPayload() {
		Response response = new RestRemoteContentManagerServer().update("not-gzip-base64");

		assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus());
	}

	@Test
	void putBeanReturnsOkForEncodedContent() throws Exception {
		Response response = new RestRemoteContentManagerServer().put(StateUtil.encode64(beanContent()));

		assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
		assertTrue(CapturingContentManager.beanPut);
	}

	@Test
	void putAttachmentReturnsOkForEncodedContent() throws Exception {
		AttachmentContent attachment = attachmentContent();

		Response response = new RestRemoteContentManagerServer().put(StateUtil.encode64(attachment), true);

		assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
		assertEquals("content-1", response.getEntity());
		assertTrue(CapturingContentManager.attachmentPut);
	}

	@Test
	void updateAttachmentReturnsOkForEncodedContent() throws Exception {
		Response response = new RestRemoteContentManagerServer().update(StateUtil.encode64(attachmentContent()));

		assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
		assertTrue(CapturingContentManager.attachmentUpdated);
	}

	@Test
	void getAttachmentReturnsNotFoundWhenContentManagerReturnsNull() {
		Response response = new RestRemoteContentManagerServer().getAttachment("missing");

		assertEquals(Response.Status.NOT_FOUND.getStatusCode(), response.getStatus());
	}

	@Test
	void getAttachmentReturnsEncodedContentWhenFound() throws Exception {
		CapturingContentManager.attachmentToReturn = attachmentContent();

		Response response = new RestRemoteContentManagerServer().getAttachment("content-1");

		assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
		AttachmentContent decoded = StateUtil.decode64((String) response.getEntity());
		assertEquals("content-1", decoded.getContentId());
	}

	@Test
	void getAttachmentReturnsServerErrorWhenContentManagerThrows() {
		CapturingContentManager.throwOnGet = true;

		Response response = new RestRemoteContentManagerServer().getAttachment("content-1");

		assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), response.getStatus());
	}

	@Test
	void removeMethodsReturnOk() {
		RestRemoteContentManagerServer server = new RestRemoteContentManagerServer();

		assertEquals(Response.Status.OK.getStatusCode(), server.removeBean("bean-1").getStatus());
		assertEquals(Response.Status.OK.getStatusCode(), server.removeAttachment("content-1").getStatus());
		assertTrue(CapturingContentManager.beanRemoved);
		assertTrue(CapturingContentManager.attachmentRemoved);
	}

	@Test
	void removeMethodsReturnServerErrorWhenContentManagerThrows() {
		RestRemoteContentManagerServer server = new RestRemoteContentManagerServer();

		CapturingContentManager.throwOnRemoveBean = true;
		assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), server.removeBean("bean-1").getStatus());

		CapturingContentManager.throwOnRemoveBean = false;
		CapturingContentManager.throwOnRemoveAttachment = true;
		assertEquals(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode(), server.removeAttachment("content-1").getStatus());
	}

	private static BeanContent beanContent() {
		PersistentBean bean = mock(PersistentBean.class);
		when(bean.getBizCustomer()).thenReturn("customer");
		when(bean.getBizModule()).thenReturn("admin");
		when(bean.getBizDocument()).thenReturn("User");
		when(bean.getBizDataGroupId()).thenReturn("group");
		when(bean.getBizUserId()).thenReturn("user");
		when(bean.getBizId()).thenReturn("bean-1");
		BeanContent content = new BeanContent(bean);
		content.getProperties().put("name", "Admin");
		return content;
	}

	private static AttachmentContent attachmentContent() {
		AttachmentContent content = new AttachmentContent("customer", "admin", "User", "group", "user", "bean-1", "photo");
		content.setContentId("content-1");
		content.attachment("photo.txt", MimeType.plain, "hello".getBytes(java.nio.charset.StandardCharsets.UTF_8));
		return content;
	}

	public static class CapturingContentManager extends NoOpContentManager {
		private static boolean beanPut;
		private static boolean attachmentPut;
		private static boolean attachmentUpdated;
		private static boolean beanRemoved;
		private static boolean attachmentRemoved;
		private static AttachmentContent attachmentToReturn;
		private static boolean throwOnGet;
		private static boolean throwOnRemoveBean;
		private static boolean throwOnRemoveAttachment;

		private static void reset() {
			beanPut = false;
			attachmentPut = false;
			attachmentUpdated = false;
			beanRemoved = false;
			attachmentRemoved = false;
			attachmentToReturn = null;
			throwOnGet = false;
			throwOnRemoveBean = false;
			throwOnRemoveAttachment = false;
		}

		@Override
		public void put(BeanContent content) {
			beanPut = true;
		}

		@Override
		public void put(AttachmentContent content, boolean index) {
			attachmentPut = index;
		}

		@Override
		public void update(AttachmentContent content) {
			attachmentUpdated = true;
		}

		@Override
		public AttachmentContent getAttachment(String contentId) {
			if (throwOnGet) {
				throw new IllegalStateException("get failed");
			}
			return attachmentToReturn;
		}

		@Override
		public void removeBean(String bizId) {
			if (throwOnRemoveBean) {
				throw new IllegalStateException("remove bean failed");
			}
			beanRemoved = true;
		}

		@Override
		public void removeAttachment(String contentId) {
			if (throwOnRemoveAttachment) {
				throw new IllegalStateException("remove attachment failed");
			}
			attachmentRemoved = true;
		}
	}

	private static final class TestRuntimeDelegate extends RuntimeDelegate {
		@Override
		public Response.ResponseBuilder createResponseBuilder() {
			return new TestResponseBuilder();
		}

		@Override
		public UriBuilder createUriBuilder() {
			throw new UnsupportedOperationException();
		}

		@Override
		public Variant.VariantListBuilder createVariantListBuilder() {
			throw new UnsupportedOperationException();
		}

		@Override
		public <T> T createEndpoint(Application application, Class<T> endpointType) {
			throw new UnsupportedOperationException();
		}

		@Override
		public <T> HeaderDelegate<T> createHeaderDelegate(Class<T> type) {
			throw new UnsupportedOperationException();
		}

		@Override
		public Link.Builder createLinkBuilder() {
			throw new UnsupportedOperationException();
		}

		@Override
		public SeBootstrap.Configuration.Builder createConfigurationBuilder() {
			throw new UnsupportedOperationException();
		}

		@Override
		public CompletionStage<SeBootstrap.Instance> bootstrap(Application application, SeBootstrap.Configuration configuration) {
			throw new UnsupportedOperationException();
		}

		@Override
		public CompletionStage<SeBootstrap.Instance> bootstrap(Class<? extends Application> applicationClass, SeBootstrap.Configuration configuration) {
			throw new UnsupportedOperationException();
		}

		@Override
		public EntityPart.Builder createEntityPartBuilder(String partName) throws IllegalArgumentException {
			throw new UnsupportedOperationException();
		}
	}

	private static final class TestResponseBuilder extends Response.ResponseBuilder {
		private int status = Response.Status.OK.getStatusCode();
		private Object entity;
		private final MultivaluedMap<String, Object> headers = new MultivaluedHashMap<>();

		@Override
		public Response build() {
			return new TestResponse(status, entity, headers);
		}

		@Override
		public Response.ResponseBuilder clone() {
			TestResponseBuilder clone = new TestResponseBuilder();
			clone.status = status;
			clone.entity = entity;
			clone.headers.putAll(headers);
			return clone;
		}

		@Override
		public Response.ResponseBuilder status(@SuppressWarnings("hiding") int status) {
			this.status = status;
			return this;
		}

		@Override
		public Response.ResponseBuilder status(@SuppressWarnings("hiding") int status, String reasonPhrase) {
			return status(status);
		}

		@Override
		public Response.ResponseBuilder entity(@SuppressWarnings("hiding") Object entity) {
			this.entity = entity;
			return this;
		}

		@Override
		public Response.ResponseBuilder entity(@SuppressWarnings("hiding") Object entity, Annotation[] annotations) {
			return entity(entity);
		}

		@Override
		public Response.ResponseBuilder header(String name, Object value) {
			headers.add(name, value);
			return this;
		}

		@Override public Response.ResponseBuilder allow(String... methods) { return this; }
		@Override public Response.ResponseBuilder allow(Set<String> methods) { return this; }
		@Override public Response.ResponseBuilder cacheControl(CacheControl cacheControl) { return this; }
		@Override public Response.ResponseBuilder encoding(String encoding) { return this; }
		@Override public Response.ResponseBuilder replaceAll(@SuppressWarnings("hiding") MultivaluedMap<String, Object> headers) { this.headers.clear(); this.headers.putAll(headers); return this; }
		@Override public Response.ResponseBuilder language(String language) { return this; }
		@Override public Response.ResponseBuilder language(Locale language) { return this; }
		@Override public Response.ResponseBuilder type(MediaType type) { return this; }
		@Override public Response.ResponseBuilder type(String type) { return this; }
		@Override public Response.ResponseBuilder variant(Variant variant) { return this; }
		@Override public Response.ResponseBuilder contentLocation(URI location) { return this; }
		@Override public Response.ResponseBuilder cookie(NewCookie... cookies) { return this; }
		@Override public Response.ResponseBuilder expires(Date expires) { return this; }
		@Override public Response.ResponseBuilder lastModified(Date lastModified) { return this; }
		@Override public Response.ResponseBuilder location(URI location) { return this; }
		@Override public Response.ResponseBuilder tag(EntityTag tag) { return this; }
		@Override public Response.ResponseBuilder tag(String tag) { return this; }
		@Override public Response.ResponseBuilder variants(Variant... variants) { return this; }
		@Override public Response.ResponseBuilder variants(List<Variant> variants) { return this; }
		@Override public Response.ResponseBuilder links(Link... links) { return this; }
		@Override public Response.ResponseBuilder link(URI uri, String rel) { return this; }
		@Override public Response.ResponseBuilder link(String uri, String rel) { return this; }
	}

	private static final class TestResponse extends Response {
		private final int status;
		private final Object entity;
		private final MultivaluedMap<String, Object> headers;

		private TestResponse(int status, Object entity, MultivaluedMap<String, Object> headers) {
			this.status = status;
			this.entity = entity;
			this.headers = headers;
		}

		@Override public int getStatus() { return status; }
		@Override public StatusType getStatusInfo() { return Response.Status.fromStatusCode(status); }
		@Override public Object getEntity() { return entity; }
		@Override public boolean hasEntity() { return entity != null; }
		@Override public MultivaluedMap<String, Object> getMetadata() { return headers; }
		@Override public MultivaluedMap<String, Object> getHeaders() { return getMetadata(); }
		@Override public MultivaluedMap<String, String> getStringHeaders() { return new MultivaluedHashMap<>(); }
		@Override public String getHeaderString(String name) { return null; }
		@Override public void close() { 
			// nothing to see here
		}
		@Override public <T> T readEntity(Class<T> entityType) { return entityType.cast(entity); }
		@Override public <T> T readEntity(GenericType<T> entityType) { throw new UnsupportedOperationException(); }
		@Override public <T> T readEntity(Class<T> entityType, Annotation[] annotations) { return readEntity(entityType); }
		@Override public <T> T readEntity(GenericType<T> entityType, Annotation[] annotations) { throw new UnsupportedOperationException(); }
		@Override public boolean bufferEntity() { return false; }
		@Override public MediaType getMediaType() { return null; }
		@Override public Locale getLanguage() { return null; }
		@Override public int getLength() { return -1; }
		@Override public Set<String> getAllowedMethods() { return Collections.emptySet(); }
		@Override public Map<String, NewCookie> getCookies() { return Collections.emptyMap(); }
		@Override public EntityTag getEntityTag() { return null; }
		@Override public Date getDate() { return null; }
		@Override public Date getLastModified() { return null; }
		@Override public URI getLocation() { return null; }
		@Override public Set<Link> getLinks() { return Collections.emptySet(); }
		@Override public boolean hasLink(String relation) { return false; }
		@Override public Link getLink(String relation) { return null; }
		@Override public Link.Builder getLinkBuilder(String relation) { return null; }
	}
}
