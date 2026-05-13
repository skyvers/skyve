package org.skyve.metadata.router.fluent;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.List;


import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.router.Route;
import org.skyve.impl.metadata.repository.router.RouteCriteria;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.WebAction;

@SuppressWarnings("static-method")
class FluentRouterTest {
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

@Test
void routeManagesCriteriaAndCopiesFromSource() {
FluentRouteCriteria first = criteria("sales", "Order", "bizhub", "q1", ViewType.edit, "user1", "group1", WebAction.e);
FluentRouteCriteria second = criteria("admin", "Audit", "bizhub", "q2", ViewType.list, "user2", "group2", WebAction.l);
FluentRoute route = new FluentRoute().outcomeUrl("/orders")
.addCriteria(first)
.addCriteria(second);

List<FluentRouteCriteria> found = route.findCriteria(criteria("sales", null, null, null, null, null, null, null));
assertThat(found.size(), is(1));
assertThat(found.get(0).get().getDocumentName(), is("Order"));

route.removeCriteria(criteria("sales", null, null, null, null, null, null, null));
assertThat(route.get().getCriteria().size(), is(1));
assertThat(route.get().getCriteria().get(0).getModuleName(), is("admin"));

Route source = new Route();
source.setOutcomeUrl("/copied");
source.getCriteria().add(first.get());
FluentRoute copied = new FluentRoute().from(source);
assertThat(copied.get().getOutcomeUrl(), is("/copied"));
assertThat(copied.get().getCriteria().size(), is(1));

route.clearCriteria();
assertThat(route.get().getCriteria().isEmpty(), is(true));
}

@Test
void uxUiManagesRoutesAndCopiesFromSource() {
FluentRoute orders = new FluentRoute().outcomeUrl("/orders");
FluentRoute audit = new FluentRoute().outcomeUrl("/audit");
FluentUxUi uxui = new FluentUxUi().name("desktop").addRoute(orders).addRoute(audit);

assertThat(uxui.findRoutes("/orders").size(), is(1));
uxui.removeRoutes("/orders");
assertThat(uxui.get().getRoutes().size(), is(1));

UxUiMetadata source = new UxUiMetadata();
source.setName("mobile");
source.getRoutes().add(audit.get());
FluentUxUi copied = new FluentUxUi().from(source);
assertThat(copied.get().getName(), is("mobile"));
assertThat(copied.findRoutes("/audit").size(), is(1));

uxui.clearRoutes();
assertThat(uxui.get().getRoutes().isEmpty(), is(true));
}

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
assertThat(router.get().getUnsecuredUrlPrefixes().size(), is(1));
		assertThat(router.get().getUnsecuredUrlPrefixes().contains("/health"), is(true));

Router source = new Router();
source.setUxuiSelectorClassName("org.skyve.CopiedSelector");
source.getUxUis().add(mobile.get());
source.getUnsecuredUrlPrefixes().add("/status");
FluentRouter copied = new FluentRouter().from(source);
assertThat(copied.get().getUxuiSelectorClassName(), is("org.skyve.CopiedSelector"));
assertThat(copied.findUxUi("mobile"), is(notNullValue()));
assertThat(copied.get().getUnsecuredUrlPrefixes().size(), is(1));
		assertThat(copied.get().getUnsecuredUrlPrefixes().contains("/status"), is(true));

router.clearUxUis().clearUnsecuredUrlPrefixes();
assertThat(router.get().getUxUis().isEmpty(), is(true));
assertThat(router.get().getUnsecuredUrlPrefixes().isEmpty(), is(true));
}

@Test
void findUxUiReturnsWrappedMetadata() {
FluentRouter router = new FluentRouter().addUxUi(new FluentUxUi().name("desktop"));

FluentUxUi found = router.findUxUi("desktop");

assertThat(found, is(notNullValue()));
assertThat(found, is(instanceOf(FluentUxUi.class)));
}

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
