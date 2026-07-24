package org.skyve.metadata.router.fluent;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.router.Direct;
import org.skyve.impl.metadata.repository.router.Direct.DirectMatch;
import org.skyve.impl.metadata.repository.router.Route;
import org.skyve.impl.metadata.repository.router.RouteCriteria;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.UserAgentType;
import org.skyve.web.WebAction;

/** Exercises fluent router builders for criteria, route, and UX/UI copy and mutation paths. */
@SuppressWarnings("static-method")
class FluentRouterTest {
	/** Verifies that {@link FluentRouteCriteria#from(RouteCriteria)} copies all route criteria fields. */
	@Test
	void routeCriteriaFromCopiesAllFields() {
		RouteCriteria source = new RouteCriteria();
		source.setViewType(ViewType.edit);
		source.setWebAction(WebAction.e);
		source.setModuleName("sales");
		source.setDocumentName("Order");
		source.setQueryName("qOrders");
		source.setCustomerName("bizhub");
		source.setDataGroupId("group1");
		source.setUserId("user1");

		FluentRouteCriteria fluent = new FluentRouteCriteria().from(source);

		assertThat(fluent.get().getViewType(), is(ViewType.edit));
		assertThat(fluent.get().getWebAction(), is(WebAction.e));
		assertThat(fluent.get().getModuleName(), is("sales"));
		assertThat(fluent.get().getDocumentName(), is("Order"));
		assertThat(fluent.get().getQueryName(), is("qOrders"));
		assertThat(fluent.get().getCustomerName(), is("bizhub"));
		assertThat(fluent.get().getDataGroupId(), is("group1"));
		assertThat(fluent.get().getUserId(), is("user1"));
	}

	/** Verifies that fluent routes manage criteria collections and copy from route metadata. */
	@Test
	void routeManagesCriteriaAndCopiesFromSource() {
		FluentRouteCriteria first = criteria("sales", "Order", "bizhub", "q1", ViewType.edit, "user1", "group1", WebAction.e);
		FluentRouteCriteria second = criteria("admin", "Audit", "bizhub", "q2", ViewType.list, "user2", "group2", WebAction.l);
		FluentRoute route = new FluentRoute().outcomeUrl("/orders")
				.addCriteria(first)
				.addCriteria(second);

		List<FluentRouteCriteria> found = route.findCriteria(criteria("sales", null, null, null, null, null, null, null));
		assertEquals(1, found.size());
		assertThat(found.get(0).get().getDocumentName(), is("Order"));

		route.removeCriteria(criteria("sales", null, null, null, null, null, null, null));
		assertEquals(1, route.get().getCriteria().size());
		assertThat(route.get().getCriteria().get(0).getModuleName(), is("admin"));

		Route source = new Route();
		source.setOutcomeUrl("/copied");
		source.getCriteria().add(first.get());
		FluentRoute copied = new FluentRoute().from(source);
		assertThat(copied.get().getOutcomeUrl(), is("/copied"));
		assertEquals(1, copied.get().getCriteria().size());

		route.clearCriteria();
		assertTrue(route.get().getCriteria().isEmpty());
	}

	/** Verifies that fluent UX/UI metadata manages routes and copies from source metadata. */
	@Test
	void uxUiManagesRoutesAndCopiesFromSource() {
		FluentRoute orders = new FluentRoute().outcomeUrl("/orders");
		FluentRoute audit = new FluentRoute().outcomeUrl("/audit");
		FluentUxUi uxui = new FluentUxUi().name("desktop").addRoute(orders).addRoute(audit);

		assertEquals(1, uxui.findRoutes("/orders").size());
		uxui.removeRoutes("/orders");
		assertEquals(1, uxui.get().getRoutes().size());

		UxUiMetadata source = new UxUiMetadata();
		source.setName("mobile");
		source.getRoutes().add(audit.get());
		FluentUxUi copied = new FluentUxUi().from(source);
		assertThat(copied.get().getName(), is("mobile"));
		assertEquals(1, copied.findRoutes("/audit").size());

		uxui.clearRoutes();
		assertTrue(uxui.get().getRoutes().isEmpty());
	}

	/** Verifies that fluent routers manage UX/UIs, unsecured prefixes, and copy source metadata. */
	@Test
	void routerManagesUxUisAndUnsecuredPrefixesAndCopiesFromSource() {
		FluentUxUi desktop = new FluentUxUi().name("desktop");
		FluentUxUi mobile = new FluentUxUi().name("mobile");
		FluentRouter router = new FluentRouter().uxuiSelectorClassName("org.skyve.RouterSelector")
				.addUxUi(desktop)
				.addUxUi(mobile)
				.addUnsecuredUrlPrefix("/public")
				.addUnsecuredUrlPrefix("/health");

		assertThat(router.findUxUi("desktop"), is(notNullValue()));
		router.removeUxUi("desktop");
		assertThat(router.findUxUi("desktop"), is(nullValue()));
		router.removeUnsecuredUrlPrefix("/public");
		assertEquals(1, router.get().getUnsecuredUrlPrefixes().size());
		assertTrue(router.get().getUnsecuredUrlPrefixes().contains("/health"));

		Router source = new Router();
		source.setUxuiSelectorClassName("org.skyve.CopiedSelector");
		source.getUxUis().add(mobile.get());
		source.getUnsecuredUrlPrefixes().add("/status");
		FluentRouter copied = new FluentRouter().from(source);
		assertThat(copied.get().getUxuiSelectorClassName(), is("org.skyve.CopiedSelector"));
		assertThat(copied.findUxUi("mobile"), is(notNullValue()));
		assertEquals(1, copied.get().getUnsecuredUrlPrefixes().size());
		assertTrue(copied.get().getUnsecuredUrlPrefixes().contains("/status"));

		router.clearUxUis().clearUnsecuredUrlPrefixes();
		assertTrue(router.get().getUxUis().isEmpty());
		assertTrue(router.get().getUnsecuredUrlPrefixes().isEmpty());
	}

	/** Verifies that router lookup returns wrapped fluent UX/UI metadata. */
	@Test
	void findUxUiReturnsWrappedMetadata() {
		FluentRouter router = new FluentRouter().addUxUi(new FluentUxUi().name("desktop"));

		FluentUxUi found = router.findUxUi("desktop");

		assertThat(found, is(notNullValue()));
		assertThat(found, is(instanceOf(FluentUxUi.class)));
	}

	/** Verifies that the wrapping constructor uses the supplied Router instance. */
	@Test
	void wrappingConstructorPreservesInstance() {
		Router existing = new Router();
		FluentRouter fluent = new FluentRouter(existing);
		assertThat(fluent.get(), is(existing));
	}

	/** Verifies that fluent direct setters and copying preserve every metadata value. */
	@Test
	void directSetsAndCopiesEveryValue() {
		FluentDirect source = new FluentDirect().path(" /external/ ")
				.uxui(" external ")
				.match(DirectMatch.prefix)
				.userAgentType(UserAgentType.phone);

		FluentDirect copied = new FluentDirect().from(source.get());

		assertThat(copied.get().getPath(), is("/external/"));
		assertThat(copied.get().getUxui(), is("external"));
		assertThat(copied.get().getMatch(), is(DirectMatch.prefix));
		assertThat(copied.get().getUserAgentType(), is(UserAgentType.phone));
	}

	/** Verifies ordered all-match find, remove, clear, and source-copy direct semantics. */
	@Test
	void routerManagesAllDuplicateDirectsInDeclarationOrder() {
		FluentDirect first = new FluentDirect().path("/same.xhtml").uxui("first");
		FluentDirect other = new FluentDirect().path("/other.xhtml").uxui("other");
		FluentDirect second = new FluentDirect().path("/same.xhtml")
				.uxui("second")
				.match(DirectMatch.prefix);
		FluentRouter fluent = new FluentRouter().addDirect(first).addDirect(other).addDirect(second);

		List<FluentDirect> found = fluent.findDirects("  /same.xhtml  ");
		assertEquals(2, found.size());
		assertThat(found.get(0).get().getUxui(), is("first"));
		assertThat(found.get(1).get().getUxui(), is("second"));

		Router source = fluent.get();
		FluentRouter copied = new FluentRouter().from(source);
		assertEquals(3, copied.get().getDirects().size());
		assertThat(copied.get().getDirects().get(1).getUxui(), is("other"));
		assertThat(copied.get().getDirects().get(2).getMatch(), is(DirectMatch.prefix));

		fluent.removeDirects(" /same.xhtml ");
		assertEquals(1, fluent.get().getDirects().size());
		assertThat(fluent.get().getDirects().get(0).getUxui(), is("other"));
		fluent.clearDirects();
		assertTrue(fluent.get().getDirects().isEmpty());
	}

	/** Verifies that the wrapping constructor retains a supplied direct instance. */
	@Test
	void directWrappingConstructorPreservesInstance() {
		Direct existing = new Direct();
		FluentDirect fluent = new FluentDirect(existing);
		assertThat(fluent.get(), is(existing));
	}

	/** Builds route criteria with the supplied field values for add, find, and remove assertions. */
	private static FluentRouteCriteria criteria(String moduleName,
			String documentName,
			String customerName,
			String queryName,
			ViewType viewType,
			String userId,
			String dataGroupId,
			WebAction webAction) {
		return new FluentRouteCriteria().moduleName(moduleName)
				.documentName(documentName)
				.customerName(customerName)
				.queryName(queryName)
				.viewType(viewType)
				.userId(userId)
				.dataGroupId(dataGroupId)
				.webAction(webAction);
	}
}
