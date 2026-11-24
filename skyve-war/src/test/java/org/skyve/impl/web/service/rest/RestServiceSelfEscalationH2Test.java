package org.skyve.impl.web.service.rest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Field;
import java.util.Objects;
import java.nio.file.Path;

import javax.xml.parsers.DocumentBuilderFactory;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.web.filter.rest.ForbiddenFilter;
import org.skyve.metadata.user.User;
import org.skyve.util.DataBuilder;
import org.skyve.util.JSON;
import org.skyve.util.test.SkyveFixture.FixtureType;
import org.springframework.mock.web.MockFilterChain;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import jakarta.servlet.http.HttpServletResponse;
import modules.admin.User.UserExtension;
import modules.admin.domain.AdminDocument;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

/**
 * The REST service's update route, driven as an {@code AppUser} (who holds {@code admin.User} {@code _RU_U}:
 * update, own record only). The route is blocked by default in {@code web.xml}; this guards that block and
 * records what is behind it.
 */
@SuppressWarnings("static-method")
class RestServiceSelfEscalationH2Test extends AbstractH2Test {
	@Test
	void webXmlBlocksTheRestRoutesByDefault() throws Exception {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
		Document document = factory.newDocumentBuilder().parse(Path.of("src/main/webapp/WEB-INF/web.xml").toFile());
		String filterName = null;
		NodeList mappings = document.getElementsByTagName("filter-mapping");
		for (int i = 0; i < mappings.getLength(); i++) {
			Element mapping = (Element) mappings.item(i);
			NodeList patterns = mapping.getElementsByTagName("url-pattern");
			if ((patterns.getLength() > 0) && "/rest/*".equals(patterns.item(0).getTextContent().trim())) {
				filterName = mapping.getElementsByTagName("filter-name").item(0).getTextContent().trim();
			}
		}
		assertEquals("RestFilter", filterName);
		NodeList filters = document.getElementsByTagName("filter");
		String filterClass = null;
		for (int i = 0; i < filters.getLength(); i++) {
			Element filter = (Element) filters.item(i);
			if ("RestFilter".equals(filter.getElementsByTagName("filter-name").item(0).getTextContent().trim())) {
				filterClass = filter.getElementsByTagName("filter-class").item(0).getTextContent().trim();
			}
		}
		assertEquals(ForbiddenFilter.class.getName(), filterClass);
	}

	@Test
	void theForbiddenFilterAnswers403WithoutReachingTheService() throws Exception {
		ForbiddenFilter filter = new ForbiddenFilter();
		org.springframework.mock.web.MockFilterConfig config = new org.springframework.mock.web.MockFilterConfig();
		filter.init(config);
		MockHttpServletResponse response = new MockHttpServletResponse();
		MockFilterChain chain = new MockFilterChain();

		MockHttpServletRequest request = new MockHttpServletRequest("POST", "/rest/api/json/update");
		request.setServletPath("/rest");
		request.setPathInfo("/api/json/update");
		filter.doFilter(request, response, chain);

		assertEquals(403, response.getStatus());
		assertEquals(null, chain.getRequest());
	}

	@Test
	@Disabled("BUG-4: REST write routes check no permissions; /rest/* is forbidden by default (ForbiddenFilter); "
			+ "known, not fixed by Mike's decision (2026-11-19)")
	void anAppUserCannotGiveThemselfARoleThroughTheRestUpdate() throws Exception {
		AbstractPersistence p = AbstractPersistence.get();
		User original = p.getUser();
		DataBuilder builder = new DataBuilder().fixture(FixtureType.crud);
		UserExtension me = builder.build(AdminDocument.USER);
		me.setUserName("rest-escalate-me");
		me.getGroups().clear();
		me.getRoles().clear();
		UserRole role = builder.build(AdminDocument.USER_ROLE);
		role.setRoleName("admin.AppUser");
		me.addRolesElement(role);
		me = p.save(me);
		p.commit(false);
		p.begin();
		User asAppUser = me.toMetaDataUser();
		p.setUser(asAppUser);
		try {
			UserExtension mine = Objects.requireNonNull(p.retrieve(AdminDocument.USER, me.getBizId()));
			UserRole extra = UserRole.newInstance();
			extra.setRoleName("admin.SecurityAdministrator");
			mine.addRolesElement(extra);
			String json = JSON.marshall(asAppUser.getCustomer(), mine);
			HttpServletResponse response = mock(HttpServletResponse.class);
			RestService service = new RestService();
			Field responseField = RestService.class.getDeclaredField("response");
			responseField.setAccessible(true);
			responseField.set(service, response);

			service.updateJSONPost(json);

			p.setUser(original);
			p.commit(false);
			p.begin();
			Number roles = p.newSQL(
					"select count(*) from ADM_SecurityUserRole r where r.parent_id = :id and r.roleName like '%SecurityAdministrator'")
					.putParameter("id", me.getBizId(), false)
					.scalarResult(Number.class);
			assertEquals(0, Objects.requireNonNull(roles).intValue(), "the AppUser now holds SecurityAdministrator");
		} finally {
			p.setUser(original);
		}
	}
}
