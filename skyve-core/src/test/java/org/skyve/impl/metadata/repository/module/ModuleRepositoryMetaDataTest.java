package org.skyve.impl.metadata.repository.module;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.user.DocumentPermission;
import org.skyve.metadata.view.View.ViewType;

@SuppressWarnings({"static-method", "boxing"})
class ModuleRepositoryMetaDataTest {

	// ModuleMetaData

	@Test
	void moduleTitleRoundTrips() {
		ModuleMetaData m = new ModuleMetaData();
		m.setTitle("My Module");
		assertThat(m.getTitle(), is("My Module"));
	}

	@Test
	void moduleNameRoundTrips() {
		ModuleMetaData m = new ModuleMetaData();
		m.setName("admin");
		assertThat(m.getName(), is("admin"));
	}

	@Test
	void modulePrototypeRoundTrips() {
		ModuleMetaData m = new ModuleMetaData();
		m.setPrototype(Boolean.TRUE);
		assertThat(m.getPrototype(), is(Boolean.TRUE));
	}

	@Test
	void modulePrototypeDefaultIsNull() {
		ModuleMetaData m = new ModuleMetaData();
		assertThat(m.getPrototype(), nullValue());
	}

	@Test
	void moduleHomeDocumentRoundTrips() {
		ModuleMetaData m = new ModuleMetaData();
		m.setHomeDocument("Contact");
		assertThat(m.getHomeDocument(), is("Contact"));
	}

	@Test
	void moduleHomeRefRoundTrips() {
		ModuleMetaData m = new ModuleMetaData();
		m.setHomeRef(ViewType.edit);
		assertThat(m.getHomeRef(), is(ViewType.edit));
	}

	@Test
	void moduleFormLabelLayoutRoundTrips() {
		ModuleMetaData m = new ModuleMetaData();
		m.setFormLabelLayout(FormLabelLayout.top);
		assertThat(m.getFormLabelLayout(), is(FormLabelLayout.top));
	}

	@Test
	void moduleDocumentsNotNull() {
		ModuleMetaData m = new ModuleMetaData();
		assertThat(m.getDocuments(), notNullValue());
	}

	@Test
	void moduleRolesNotNull() {
		ModuleMetaData m = new ModuleMetaData();
		assertThat(m.getRoles(), notNullValue());
	}

	@Test
	void moduleJobsNotNull() {
		ModuleMetaData m = new ModuleMetaData();
		assertThat(m.getJobs(), notNullValue());
	}

	@Test
	void modulePropertiesNotNull() {
		ModuleMetaData m = new ModuleMetaData();
		assertThat(m.getProperties(), notNullValue());
	}

	// ModuleRoleMetaData

	@Test
	void moduleRoleNameRoundTrips() {
		ModuleRoleMetaData r = new ModuleRoleMetaData();
		r.setName("BasicUser");
		assertThat(r.getName(), is("BasicUser"));
	}

	@Test
	void moduleRoleDescriptionRoundTrips() {
		ModuleRoleMetaData r = new ModuleRoleMetaData();
		r.setDescription("A basic user role");
		assertThat(r.getDescription(), is("A basic user role"));
	}

	@Test
	void moduleRoleDocumentationRoundTrips() {
		ModuleRoleMetaData r = new ModuleRoleMetaData();
		r.setDocumentation("Role docs");
		assertThat(r.getDocumentation(), is("Role docs"));
	}

	@Test
	void moduleRolePrivilegesNotNull() {
		ModuleRoleMetaData r = new ModuleRoleMetaData();
		assertThat(r.getPrivileges(), notNullValue());
	}

	@Test
	void moduleRoleAccessesNotNull() {
		ModuleRoleMetaData r = new ModuleRoleMetaData();
		assertThat(r.getAccesses(), notNullValue());
	}

	@Test
	void moduleRolePropertiesNotNull() {
		ModuleRoleMetaData r = new ModuleRoleMetaData();
		assertThat(r.getProperties(), notNullValue());
	}

	// DocumentPrivilegeMetaData

	@Test
	void documentPrivilegeDocumentNameRoundTrips() {
		DocumentPrivilegeMetaData dp = new DocumentPrivilegeMetaData();
		dp.setDocumentName("Contact");
		assertThat(dp.getDocumentName(), is("Contact"));
	}

	@Test
	void documentPrivilegePermissionRoundTrips() {
		DocumentPrivilegeMetaData dp = new DocumentPrivilegeMetaData();
		dp.setPermission(DocumentPermission.CRUDC);
		assertThat(dp.getPermission(), is(DocumentPermission.CRUDC));
	}

	@Test
	void documentPrivilegeActionsNotNull() {
		DocumentPrivilegeMetaData dp = new DocumentPrivilegeMetaData();
		assertThat(dp.getActions(), notNullValue());
	}

	@Test
	void documentPrivilegeContentRestrictionsNotNull() {
		DocumentPrivilegeMetaData dp = new DocumentPrivilegeMetaData();
		assertThat(dp.getContentRestrictions(), notNullValue());
	}

	@Test
	void documentPrivilegeContentPermissionsNotNull() {
		DocumentPrivilegeMetaData dp = new DocumentPrivilegeMetaData();
		assertThat(dp.getContentPermissions(), notNullValue());
	}

	@Test
	void documentPrivilegePropertiesNotNull() {
		DocumentPrivilegeMetaData dp = new DocumentPrivilegeMetaData();
		assertThat(dp.getProperties(), notNullValue());
	}

	// ActionPrivilegeMetaData

	@Test
	void actionPrivilegeActionNameRoundTrips() {
		ActionPrivilegeMetaData ap = new ActionPrivilegeMetaData();
		ap.setActionName("Save");
		assertThat(ap.getActionName(), is("Save"));
	}

	@Test
	void actionPrivilegePropertiesNotNull() {
		ActionPrivilegeMetaData ap = new ActionPrivilegeMetaData();
		assertThat(ap.getProperties(), notNullValue());
	}

	@Test
	void actionPrivilegeBlankNameBecomesNull() {
		ActionPrivilegeMetaData ap = new ActionPrivilegeMetaData();
		ap.setActionName("  ");
		assertThat(ap.getActionName(), nullValue());
	}

	// ModuleDocumentMetaData

	@Test
	void moduleDocumentRefRoundTrips() {
		ModuleDocumentMetaData md = new ModuleDocumentMetaData();
		md.setRef("Contact");
		assertThat(md.getRef(), is("Contact"));
	}

	@Test
	void moduleDocumentDefaultQueryNameRoundTrips() {
		ModuleDocumentMetaData md = new ModuleDocumentMetaData();
		md.setDefaultQueryName("qContacts");
		assertThat(md.getDefaultQueryName(), is("qContacts"));
	}

	@Test
	void moduleDocumentModuleRefRoundTrips() {
		ModuleDocumentMetaData md = new ModuleDocumentMetaData();
		md.setModuleRef("contacts");
		assertThat(md.getModuleRef(), is("contacts"));
	}

	@Test
	void moduleDocumentPropertiesNotNull() {
		ModuleDocumentMetaData md = new ModuleDocumentMetaData();
		assertThat(md.getProperties(), notNullValue());
	}

	// MetaDataQueryMetaData

	@Test
	void metaDataQueryDocumentNameRoundTrips() {
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		q.setDocumentName("Contact");
		assertThat(q.getDocumentName(), is("Contact"));
	}

	@Test
	void metaDataQueryPolymorphicRoundTrips() {
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		q.setPolymorphic(Boolean.TRUE);
		assertThat(q.getPolymorphic(), is(Boolean.TRUE));
	}

	@Test
	void metaDataQueryAggregateRoundTrips() {
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		q.setAggregate(Boolean.FALSE);
		assertThat(q.getAggregate(), is(Boolean.FALSE));
	}

	@Test
	void metaDataQueryFromRoundTrips() {
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		q.setFrom("admin.Contact AS bean");
		assertThat(q.getFrom(), is("admin.Contact AS bean"));
	}

	@Test
	void metaDataQueryFilterRoundTrips() {
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		q.setFilter("bean.active = true");
		assertThat(q.getFilter(), is("bean.active = true"));
	}

	@Test
	void metaDataQueryGroupingRoundTrips() {
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		q.setGrouping("bean.category");
		assertThat(q.getGrouping(), is("bean.category"));
	}

	@Test
	void metaDataQueryOrderingRoundTrips() {
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		q.setOrdering("bean.name ASC");
		assertThat(q.getOrdering(), is("bean.name ASC"));
	}

	@Test
	void metaDataQueryColumnsNotNull() {
		MetaDataQueryMetaData q = new MetaDataQueryMetaData();
		assertThat(q.getColumns(), notNullValue());
	}

	// QueryDefinitionMetaData (via BizQLMetaData)

	@Test
	void queryDefinitionDescriptionRoundTrips() {
		BizQLMetaData q = new BizQLMetaData();
		q.setDescription("BizQL desc");
		assertThat(q.getDescription(), is("BizQL desc"));
	}

	@Test
	void queryDefinitionDocumentationRoundTrips() {
		BizQLMetaData q = new BizQLMetaData();
		q.setDocumentation("BizQL docs");
		assertThat(q.getDocumentation(), is("BizQL docs"));
	}

	@Test
	void queryDefinitionTimeoutRoundTrips() {
		BizQLMetaData q = new BizQLMetaData();
		q.setTimeoutInSeconds(30);
		assertEquals(30, q.getTimeoutInSeconds());
	}

	// BizQLMetaData

	@Test
	void bizQLQueryRoundTrips() {
		BizQLMetaData q = new BizQLMetaData();
		q.setQuery("SELECT bean FROM admin.Contact AS bean");
		assertThat(q.getQuery(), is("SELECT bean FROM admin.Contact AS bean"));
	}

	@Test
	void bizQLQueryBlankBecomesNull() {
		BizQLMetaData q = new BizQLMetaData();
		q.setQuery("  ");
		assertThat(q.getQuery(), nullValue());
	}

	// SQLMetaData

	@Test
	void sqlQueryRoundTrips() {
		SQLMetaData q = new SQLMetaData();
		q.setQuery("SELECT * FROM ADM_Contact");
		assertThat(q.getQuery(), is("SELECT * FROM ADM_Contact"));
	}

	@Test
	void sqlQueryNameRoundTrips() {
		SQLMetaData q = new SQLMetaData();
		q.setName("qSQL");
		assertThat(q.getName(), is("qSQL"));
	}

	// MetaDataQueryColumnMetaData (via MetaDataQueryProjectedColumnMetaData)

	@Test
	void queryColumnBindingRoundTrips() {
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setBinding("name");
		assertThat(col.getBinding(), is("name"));
	}

	@Test
	void queryColumnDisplayNameRoundTrips() {
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setDisplayName("Full Name");
		assertThat(col.getDisplayName(), is("Full Name"));
	}

	@Test
	void queryColumnSortOrderRoundTrips() {
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setSortOrder(SortDirection.ascending);
		assertThat(col.getSortOrder(), is(SortDirection.ascending));
	}

	@Test
	void queryColumnHiddenRoundTrips() {
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setHidden(Boolean.TRUE);
		assertThat(col.getHidden(), is(Boolean.TRUE));
	}

	@Test
	void queryColumnFilterOperatorRoundTrips() {
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setFilterOperator(FilterOperator.equal);
		assertThat(col.getFilterOperator(), is(FilterOperator.equal));
	}

	@Test
	void queryColumnFilterExpressionRoundTrips() {
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setFilterExpression("active");
		assertThat(col.getFilterExpression(), is("active"));
	}

	@Test
	void queryColumnPixelWidthRoundTrips() {
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setPixelWidth(150);
		assertEquals(150, col.getPixelWidth());
	}

	@Test
	void queryColumnAlignmentRoundTrips() {
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		col.setAlignment(HorizontalAlignment.centre);
		assertThat(col.getAlignment(), is(HorizontalAlignment.centre));
	}

	@Test
	void queryColumnPropertiesNotNull() {
		MetaDataQueryProjectedColumnMetaData col = new MetaDataQueryProjectedColumnMetaData();
		assertThat(col.getProperties(), notNullValue());
	}

	// ModuleRoleDocumentAggregateUserAccessMetaData

	@Test
	void documentAggregateAccessDocumentNameRoundTrips() {
		ModuleRoleDocumentAggregateUserAccessMetaData a = new ModuleRoleDocumentAggregateUserAccessMetaData();
		a.setDocumentName("Contact");
		assertThat(a.getDocumentName(), is("Contact"));
	}

	@Test
	void documentAggregateAccessUxuisNotNull() {
		ModuleRoleDocumentAggregateUserAccessMetaData a = new ModuleRoleDocumentAggregateUserAccessMetaData();
		assertThat(a.getUxuis(), notNullValue());
	}

	// ModuleRoleSingularUserAccessMetaData

	@Test
	void singularAccessDocumentNameRoundTrips() {
		ModuleRoleSingularUserAccessMetaData a = new ModuleRoleSingularUserAccessMetaData();
		a.setDocumentName("Contact");
		assertThat(a.getDocumentName(), is("Contact"));
	}

	// ModuleRoleReportUserAccessMetaData

	@Test
	void reportAccessModuleNameRoundTrips() {
		ModuleRoleReportUserAccessMetaData a = new ModuleRoleReportUserAccessMetaData();
		a.setModuleName("admin");
		assertThat(a.getModuleName(), is("admin"));
	}

	@Test
	void reportAccessDocumentNameRoundTrips() {
		ModuleRoleReportUserAccessMetaData a = new ModuleRoleReportUserAccessMetaData();
		a.setDocumentName("Contact");
		assertThat(a.getDocumentName(), is("Contact"));
	}

	@Test
	void reportAccessReportNameRoundTrips() {
		ModuleRoleReportUserAccessMetaData a = new ModuleRoleReportUserAccessMetaData();
		a.setReportName("ContactSummary");
		assertThat(a.getReportName(), is("ContactSummary"));
	}

	// ModuleRoleUserAccessUxUiMetadata

	@Test
	void uxuiMetadataNameRoundTrips() {
		ModuleRoleUserAccessUxUiMetadata u = new ModuleRoleUserAccessUxUiMetadata();
		u.setName("desktop");
		assertThat(u.getName(), is("desktop"));
	}

	// ContentRestriction

	@Test
	void contentRestrictionAttributeNameRoundTrips() {
		ContentRestriction cr = new ContentRestriction();
		cr.setAttributeName("photo");
		assertThat(cr.getAttributeName(), is("photo"));
	}

	@Test
	void contentRestrictionDocumentNameRoundTrips() {
		ContentRestriction cr = new ContentRestriction();
		cr.setDocumentName("Contact");
		assertThat(cr.getDocumentName(), is("Contact"));
	}

	@Test
	void contentRestrictionPropertiesNotNull() {
		ContentRestriction cr = new ContentRestriction();
		assertThat(cr.getProperties(), notNullValue());
	}

	// ContentPermission

	@Test
	void contentPermissionAttributeNameRoundTrips() {
		ContentPermission cp = new ContentPermission();
		cp.setAttributeName("resume");
		assertThat(cp.getAttributeName(), is("resume"));
	}

	@Test
	void contentPermissionDocumentNameRoundTrips() {
		ContentPermission cp = new ContentPermission();
		cp.setDocumentName("Staff");
		assertThat(cp.getDocumentName(), is("Staff"));
	}

	// GrantedTo

	@Test
	void grantedToRoleNameRoundTrips() {
		GrantedTo g = new GrantedTo();
		g.setRoleName("BasicUser");
		assertThat(g.getRoleName(), is("BasicUser"));
	}

	// MenuMetaData

	@Test
	void menuMetaDataActionsNotNull() {
		MenuMetaData menu = new MenuMetaData();
		assertThat(menu.getActions(), notNullValue());
	}

	@Test
	void menuMetaDataPropertiesNotNull() {
		MenuMetaData menu = new MenuMetaData();
		assertThat(menu.getProperties(), notNullValue());
	}
}
