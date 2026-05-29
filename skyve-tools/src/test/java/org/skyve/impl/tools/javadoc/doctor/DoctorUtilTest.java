package org.skyve.impl.tools.javadoc.doctor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.model.document.field.Text;
import org.skyve.impl.metadata.user.ActionPrivilege;
import org.skyve.impl.metadata.user.DocumentPrivilege;
import org.skyve.impl.metadata.user.Privilege;
import org.skyve.impl.metadata.user.RoleImpl;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Reference;
import org.skyve.metadata.model.document.UniqueConstraint;
import org.skyve.metadata.module.JobMetaData;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.MetaDataQueryProjectedColumn;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.user.DocumentPermission;

class DoctorUtilTest {

	private interface PrintAction {
		void write(PrintStream out) throws Exception;
	}

	private static String capture(PrintAction action) throws Exception {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		try (PrintStream printStream = new PrintStream(out, true, StandardCharsets.UTF_8)) {
			action.write(printStream);
		}
		return out.toString(StandardCharsets.UTF_8);
	}

	@Test
	void mainGeneratesOverviewAndPackageFiles(@TempDir java.nio.file.Path tempDir) throws Exception {
		UtilImpl.APPS_JAR_DIRECTORY = "/Users/mike/_/skyve/skyve-war/src/main/java/";
		java.nio.file.Files.createDirectories(tempDir.resolve("modules/admin/domain"));
		java.nio.file.Files.createDirectories(tempDir.resolve("modules/test/domain"));
		java.nio.file.Files.createDirectories(tempDir.resolve("modules/kitchensink/domain"));
		String outputRoot = tempDir.toAbsolutePath().toString() + File.separator;
		DoctorUtil.main(new String[] {outputRoot, "bizhub", "admin", "test", "kitchensink"});

		assertTrue(java.nio.file.Files.exists(tempDir.resolve("modules/overview.html")));
		assertTrue(java.nio.file.Files.exists(tempDir.resolve("modules/admin/domain/package.html")));
	}

	@Test
	void mainThrowsForUnknownCustomer(@TempDir java.nio.file.Path tempDir) {
		UtilImpl.APPS_JAR_DIRECTORY = "/Users/mike/_/skyve/skyve-war/src/main/java/";
		String outputRoot = tempDir.toAbsolutePath().toString() + File.separator;

		IllegalArgumentException error = assertThrows(IllegalArgumentException.class,
				() -> DoctorUtil.main(new String[] {outputRoot, "missingCustomer", "admin"}));

		assertTrue(error.getMessage().contains("missingCustomer"));
	}

	private static Text textAttribute(String name, int length) {
		Text text = new Text();
		text.setName(name);
		text.setDisplayName(name + " display");
		text.setDescription(name + " description");
		text.setLength(length);
		text.setUsage(UsageType.domain);
		return text;
	}

	private static Document createDocument() {
		Document document = mock(Document.class);
		Text bizKey = textAttribute(Bean.BIZ_KEY, 32);
		Text text = textAttribute("text", 42);
		when(document.getName()).thenReturn("Contact");
		when(document.getLocalisedPluralAlias()).thenReturn("Contacts");
		when(document.getLocalisedDescription()).thenReturn("Contact description");
		when(document.getPluralAlias()).thenReturn("Contacts");
		when(document.getDocumentation()).thenReturn("Document docs");
		when(document.getParentDocumentName()).thenReturn("Person");
		List<Attribute> attributes = Arrays.<Attribute>asList(bizKey, text);
		when(document.getAttributes()).thenReturn((List) attributes);

		Reference reference = mock(Reference.class);
		when(reference.getLocalisedDisplayName()).thenReturn("Account");
		when(reference.getAttributeType()).thenReturn(AttributeType.association);
		when(reference.getDocumentName()).thenReturn("Account");
		when(reference.isRequired()).thenReturn(true);
		when(reference.getLocalisedDescription()).thenReturn("Reference description");
		when(reference.getDocumentation()).thenReturn("Reference docs");
		when(reference.getUsage()).thenReturn(UsageType.both);
		when(reference.isDeprecated()).thenReturn(false);
		when(document.getReferenceNames()).thenReturn(new LinkedHashSet<>(Collections.singleton("account")));
		when(document.getReferenceByName("account")).thenReturn(reference);

		Condition condition = mock(Condition.class);
		when(condition.getDescription()).thenReturn("Condition description");
		when(condition.getDocumentation()).thenReturn("Condition docs");
		when(condition.getUsage()).thenReturn(UsageType.view);
		when(document.getConditionNames()).thenReturn(new LinkedHashSet<>(Collections.singleton("activeOnly")));
		when(document.getCondition("activeOnly")).thenReturn(condition);

		UniqueConstraint uniqueConstraint = mock(UniqueConstraint.class);
		when(uniqueConstraint.getName()).thenReturn("uq_contact");
		when(uniqueConstraint.getLocalisedDescription()).thenReturn("Unique description");
		when(uniqueConstraint.getScope()).thenReturn(UniqueConstraint.DocumentScope.customer);
		when(uniqueConstraint.getFieldNames()).thenReturn(Arrays.asList("text", "missingField"));
		when(document.getUniqueConstraints()).thenReturn(Collections.singletonList(uniqueConstraint));

		when(document.getDefinedActionNames()).thenReturn(new LinkedHashSet<>(Collections.singleton("archive")));
		return document;
	}

	private static Module createModule(Document document, MetaDataQueryDefinition query, RoleImpl role) {
		Module module = mock(Module.class);
		Map<String, Object> documentRefs = new LinkedHashMap<>();
		documentRefs.put(document.getName(), document);
		when(module.getName()).thenReturn("admin");
		when(module.getLocalisedTitle()).thenReturn("Admin");
		when(module.getDocumentation()).thenReturn("Module docs");
		when(module.getDocumentRefs()).thenReturn((Map) documentRefs);
		when(module.getMetadataQueries()).thenReturn(Collections.singletonList(query));
		when(module.getRoles()).thenReturn(Collections.singletonList(role));
		JobMetaData job = mock(JobMetaData.class);
		when(job.getName()).thenReturn("nightly");
		when(job.getLocalisedDisplayName()).thenReturn("Nightly job");
		when(job.getClassName()).thenReturn("com.example.NightlyJob");
		when(module.getJobs()).thenReturn(Collections.singletonList(job));
		when(module.getDocument(any(Customer.class), eq("Contact"))).thenReturn(document);
		return module;
	}

	private static MetaDataQueryDefinition createQuery(Document document) {
		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		when(query.getName()).thenReturn("findContact");
		when(query.getLocalisedDescription()).thenReturn("Query description");
		when(query.getDocumentation()).thenReturn("Query docs");
		when(query.getDocumentName()).thenReturn("Contact");

		MetaDataQueryProjectedColumn projected = mock(MetaDataQueryProjectedColumn.class);
		when(projected.getBinding()).thenReturn("text");
		when(projected.getName()).thenReturn("text");
		when(projected.getDisplayName()).thenReturn("Text column");
		when(projected.getFilterExpression()).thenReturn("text != null");
		when(projected.getSortOrder()).thenReturn(SortDirection.ascending);
		when(projected.getExpression()).thenReturn("bean.text");

		MetaDataQueryColumn plain = mock(MetaDataQueryColumn.class);
		when(plain.getBinding()).thenReturn(null);
		when(plain.getName()).thenReturn("missingField");
		when(plain.getDisplayName()).thenReturn("Missing column");
		when(plain.getFilterExpression()).thenReturn(null);
		when(plain.getSortOrder()).thenReturn(null);

		when(query.getColumns()).thenReturn(Arrays.asList(projected, plain));
		return query;
	}

	private static RoleImpl createRole() {
		RoleImpl role = mock(RoleImpl.class);
		DocumentPrivilege documentPrivilege = mock(DocumentPrivilege.class);
		DocumentPermission permission = DocumentPermission._R__G;
		when(documentPrivilege.getName()).thenReturn("Contact");
		when(documentPrivilege.getPermission()).thenReturn(permission);

		ActionPrivilege actionPrivilege = mock(ActionPrivilege.class);
		when(actionPrivilege.getName()).thenReturn("archive");
		when(actionPrivilege.getDocumentName()).thenReturn("Contact");

		List<Privilege> privileges = Arrays.asList(documentPrivilege, actionPrivilege);
		when(role.getPrivileges()).thenReturn(privileges);
		when(role.getName()).thenReturn("adminRole");
		when(role.getLocalisedDescription()).thenReturn("Role description");
		when(role.getDocumentation()).thenReturn("Role docs");
		return role;
	}

	@Test
	void renderCustomerRendersNestedDocumentation() throws Exception {
		Document document = createDocument();
		MetaDataQueryDefinition query = createQuery(document);
		RoleImpl role = createRole();
		Module module = createModule(document, query, role);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("biz hub");
		when(customer.getModules()).thenReturn(Collections.singletonList(module));

		String html = capture(out -> DoctorUtil.renderCustomer(customer, out));
		assertTrue(html.contains("Overview"));
		assertTrue(html.contains("Outline"));
		assertTrue(html.contains("Module Documents"));
		assertTrue(html.contains("Module Queries"));
		assertTrue(html.contains("Roles"));
		assertTrue(html.contains("The module Admin has the following documents defined:"));
		assertTrue(html.contains("Attributes"));
		assertTrue(html.contains("References"));
		assertTrue(html.contains("Conditions"));
		assertTrue(html.contains("UniqueConstraints"));
		assertTrue(html.contains("Actions"));
		assertTrue(html.contains("Columns"));
		assertTrue(html.contains("Document Priveleges"));
		assertTrue(html.contains("<div style=\"text-align:center;\">X</div>"));
		assertTrue(html.contains("text description"));
		assertTrue(html.contains("42"));
		assertTrue(html.contains("Contact is a child of Person"));
		assertTrue(html.contains("Nightly job"));
		assertTrue(html.contains("archive"));
		assertTrue(html.contains("bean.text"));
		assertTrue(html.contains("text != null"));
	}

	@Test
	void createIdentifierSkipsEmptyFragments() {
		assertEquals("bizhub_admincontact", DoctorUtil.createIndentifier("Biz Hub", "", "Admin Contact"));
	}

	@Test
	void privateTitleCaseHandlesSingleCharacter() throws Exception {
		Method titleCase = DoctorUtil.class.getDeclaredMethod("titleCase", String.class);
		titleCase.setAccessible(true);
		assertEquals("X", titleCase.invoke(null, "x"));
	}

	@Test
	void renderQuerySupportsSqlAndBizQlHeadings() throws Exception {
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("biz hub");

		Module module = mock(Module.class);
		when(module.getName()).thenReturn("admin");

		SQLDefinition sql = mock(SQLDefinition.class);
		when(sql.getName()).thenReturn("sqlQuery");
		when(sql.getLocalisedDescription()).thenReturn("SQL description");
		when(sql.getDocumentation()).thenReturn("SQL docs");

		BizQLDefinition bizQl = mock(BizQLDefinition.class);
		when(bizQl.getName()).thenReturn("bizQlQuery");
		when(bizQl.getLocalisedDescription()).thenReturn("BizQL description");
		when(bizQl.getDocumentation()).thenReturn("BizQL docs");

		String sqlHtml = capture(out -> DoctorUtil.renderQuery(customer, module, sql, out));
		String bizQlHtml = capture(out -> DoctorUtil.renderQuery(customer, module, bizQl, out));

		assertTrue(sqlHtml.contains("SQL Query sqlQuery"));
		assertTrue(bizQlHtml.contains("BizQL Query bizQlQuery"));
	}

	@Test
	void doctorUtilClassIsInstantiable() {
		assertNotNull(new DoctorUtil());
	}
}