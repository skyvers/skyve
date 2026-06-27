package org.skyve.metadata.module.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.menu.CalendarItem;
import org.skyve.impl.metadata.module.menu.LinkItem;
import org.skyve.impl.metadata.module.menu.MapItem;
import org.skyve.impl.metadata.module.menu.TreeItem;
import org.skyve.impl.metadata.module.query.BizQLDefinitionImpl;
import org.skyve.impl.metadata.module.query.BizQLReferenceImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryContentColumnImpl;
import org.skyve.impl.metadata.module.query.MetaDataQueryReferenceImpl;
import org.skyve.impl.metadata.module.query.SQLDefinitionImpl;
import org.skyve.impl.metadata.module.query.SQLReferenceImpl;
import org.skyve.impl.metadata.repository.module.ActionPrivilegeMetaData;
import org.skyve.impl.metadata.repository.module.ContentPermission;
import org.skyve.impl.metadata.repository.module.ContentRestriction;
import org.skyve.impl.metadata.repository.module.DocumentPrivilegeMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.user.DocumentPermission;

@SuppressWarnings("static-method")
class FluentModuleAdditionalBuildersTest {

	@Test
	void bizqlAndSqlBuildersCopyQueryDefinitionProperties() {
		BizQLDefinitionImpl bizql = new BizQLDefinitionImpl();
		bizql.setName("qOrders");
		bizql.setDescription("Orders query");
		bizql.setDocumentation("BizQL docs");
		bizql.setTimeoutInSeconds(30);
		bizql.setQuery("select o from {admin.Order} o");

		FluentBizQL fluentBizQL = new FluentBizQL().from(bizql);
		assertThat(fluentBizQL.get().getName(), is("qOrders"));
		assertThat(fluentBizQL.get().getDocumentation(), is("BizQL docs"));
		assertThat(fluentBizQL.get().getQuery(), is("select o from {admin.Order} o"));

		SQLDefinitionImpl sql = new SQLDefinitionImpl();
		sql.setName("qOrderSql");
		sql.setDescription("SQL query");
		sql.setDocumentation("SQL docs");
		sql.setTimeoutInSeconds(10);
		sql.setQuery("select * from ORDERS");

		FluentSQL fluentSql = new FluentSQL().from(sql);
		assertThat(fluentSql.get().getName(), is("qOrderSql"));
		assertThat(fluentSql.get().getDescription(), is("SQL query"));
		assertThat(fluentSql.get().getQuery(), is("select * from ORDERS"));
	}

	@Test
	void queryReferenceBuildersCopyNameModuleAndReference() {
		FluentBizQLReference bizql = new FluentBizQLReference().from(new BizQLReferenceImpl("bRef", "admin", "ordersBizql"));
		assertThat(bizql.get().getName(), is("bRef"));
		assertThat(bizql.get().getModuleRef(), is("admin"));
		assertThat(bizql.get().getRef(), is("ordersBizql"));

		FluentSQLReference sql = new FluentSQLReference().from(new SQLReferenceImpl("sRef", "admin", "ordersSql"));
		assertThat(sql.get().getName(), is("sRef"));
		assertThat(sql.get().getModuleRef(), is("admin"));
		assertThat(sql.get().getRef(), is("ordersSql"));

		FluentMetaDataQueryReference metadata = new FluentMetaDataQueryReference().from(new MetaDataQueryReferenceImpl("mRef", "admin", "ordersMeta"));
		assertThat(metadata.get().getName(), is("mRef"));
		assertThat(metadata.get().getModuleRef(), is("admin"));
		assertThat(metadata.get().getRef(), is("ordersMeta"));
	}

	@Test
	void metaDataQueryContentColumnBuilderCopiesBaseAndSpecificProperties() {
		MetaDataQueryContentColumnImpl source = new MetaDataQueryContentColumnImpl();
		source.setName("thumbnail");
		source.setBinding("image");
		source.setDisplayName("Image");
		source.setSortOrder(SortDirection.ascending);
		source.setFilterOperator(FilterOperator.like);
		source.setFilterExpression("png");
		source.setHidden(true);
		source.setPixelWidth(Integer.valueOf(64));
		source.setAlignment(HorizontalAlignment.left);
		source.setDisplay(DisplayType.thumbnail);
		source.setPixelHeight(Integer.valueOf(48));
		source.setEmptyThumbnailRelativeFile("images/empty.png");

		FluentMetaDataQueryContentColumn column = new FluentMetaDataQueryContentColumn().from(source)
				.hidden(false)
				.pixelWidth(80)
				.pixelHeight(60);

		assertThat(column.get().getName(), is("thumbnail"));
		assertThat(column.get().getDisplay(), is(DisplayType.thumbnail));
		assertThat(column.get().getPixelWidth(), is(Integer.valueOf(80)));
		assertThat(column.get().getPixelHeight(), is(Integer.valueOf(60)));
	}

	@Test
	void documentPrivilegeBuilderManagesActionsPermissionsAndRestrictions() {
		DocumentPrivilegeMetaData source = new DocumentPrivilegeMetaData();
		source.setDocumentName("Order");
		source.setPermission(DocumentPermission.CRUDC);

		ActionPrivilegeMetaData action = new ActionPrivilegeMetaData();
		action.setActionName("approve");
		source.getActions().add(action);

		ContentPermission permission = new ContentPermission();
		permission.setAttributeName("amount");
		source.getContentPermissions().add(permission);

		ContentRestriction restriction = new ContentRestriction();
		restriction.setAttributeName("internalNotes");
		source.getContentRestrictions().add(restriction);

		FluentDocumentPrivilege fluent = new FluentDocumentPrivilege().from(source)
				.addActionPrivilege("cancel")
				.addContentPermission("currency")
				.addContentRestriction("auditTrail");

		assertThat(fluent.get().getDocumentName(), is("Order"));
		assertEquals(2, fluent.get().getActions().size());
		assertEquals(2, fluent.get().getContentPermissions().size());
		assertEquals(2, fluent.get().getContentRestrictions().size());

		fluent.removeActionPrivilege("approve")
				.removeContentPermission("amount")
				.removeContentRestriction("internalNotes")
				.clearActionPrivileges()
				.clearContentPermissions()
				.clearContentRestrictions();

		assertTrue(fluent.get().getActions().isEmpty());
		assertTrue(fluent.get().getContentPermissions().isEmpty());
		assertTrue(fluent.get().getContentRestrictions().isEmpty());
	}

	@Test
	void menuItemBuildersCopyRolesUxuiAndSpecificFields() {
		CalendarItem calendar = new CalendarItem();
		calendar.setName("Schedule");
		calendar.setDocumentName("Event");
		calendar.setQueryName("qEvents");
		calendar.setModelName("calendarModel");
		calendar.setStartBinding("startTime");
		calendar.setEndBinding("endTime");
		calendar.getRoleNames().add("Admin");
		calendar.getUxUis().add("desktop");

		FluentCalendarItem fluentCalendar = new FluentCalendarItem().from(calendar);
		assertThat(fluentCalendar.get().getName(), is("Schedule"));
		assertEquals(1, fluentCalendar.get().getRoles().size());
		assertEquals(1, fluentCalendar.get().getUxuis().size());

		LinkItem link = new LinkItem();
		link.setName("Support");
		link.setHref("https://example.com/support");
		link.getRoleNames().add("User");
		FluentLinkItem fluentLink = new FluentLinkItem().from(link);
		assertThat(fluentLink.get().getHref(), is("https://example.com/support"));

		MapItem map = new MapItem();
		map.setName("Sites");
		map.setDocumentName("Site");
		map.setQueryName("qSites");
		map.setModelName("siteMap");
		map.setGeometryBinding("location");
		map.setRefreshTimeInSeconds(Integer.valueOf(20));
		map.setShowRefreshControls(Boolean.TRUE);
		FluentMapItem fluentMap = new FluentMapItem().from(map);
		assertThat(fluentMap.get().getRefreshTimeInSeconds(), is(Integer.valueOf(20)));
		assertThat(fluentMap.get().getShowRefreshControls(), is(Boolean.TRUE));

		TreeItem tree = new TreeItem();
		tree.setName("Hierarchy");
		tree.setDocumentName("Category");
		tree.setQueryName("qCategories");
		tree.setModelName("categoryTree");
		tree.setAutoPopulate(true);
		FluentTreeItem fluentTree = new FluentTreeItem().from(tree);
		assertThat(fluentTree.get().getAutoPopulate(), is(Boolean.TRUE));
		assertThat(fluentTree.get().getDocumentName(), is("Category"));

		fluentCalendar.removeRole("Admin").removeUxUi("desktop").clearRoles().clearUxUis();
		assertTrue(fluentCalendar.get().getRoles().isEmpty());
		assertTrue(fluentCalendar.get().getUxuis().isEmpty());
	}

	@Test
	void moduleReportAccessBuilderSupportsUxUiMutations() {
		FluentModuleReportAccess access = new FluentModuleReportAccess().from("Order", "invoice", Set.of("desktop", "mobile"));

		assertThat(access.get(), is(notNullValue()));
		assertThat(access.get().getDocumentName(), is("Order"));
		assertThat(access.get().getImageName(), is("invoice"));
		assertEquals(2, access.get().getUxuis().size());

		access.addUxUi("tablet").removeUxUi("mobile").clearUxUis();
		assertTrue(access.get().getUxuis().isEmpty());
	}
}