package org.skyve.impl.backup;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Collection;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.content.AttachmentContent;
import org.skyve.content.ContentManager;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.persistence.SQL;

@SuppressWarnings({"static-method", "boxing"})
class ContentCheckerTest {
	private final String originalCustomer = UtilImpl.CUSTOMER;
	private final ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();

	@AfterEach
	void teardown() throws Exception {
		unbindPersistenceFromThread();
		UtilImpl.CUSTOMER = originalCustomer;
		if (originalRepository == null) {
			ProvidedRepositoryFactory.clear();
		}
		else {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	@Test
	void checkContentThrowsWhenTheDynamicEntityDocumentHasNoTableName() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		org.skyve.metadata.user.User user = mock(org.skyve.metadata.user.User.class);
		Customer customer = mock(Customer.class);
		Module admin = mock(Module.class);
		Document dynamicEntity = mock(Document.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn("demo");
		when(customer.getModule(org.skyve.domain.app.AppConstants.ADMIN_MODULE_NAME)).thenReturn(admin);
		when(admin.getDocument(customer, org.skyve.domain.app.AppConstants.DYNAMIC_ENTITY_DOCUMENT_NAME)).thenReturn(dynamicEntity);
		// persistent metadata without a name has no table name
		when(dynamicEntity.getPersistent()).thenReturn(new Persistent());

		org.skyve.metadata.MetaDataException e = assertThrows(org.skyve.metadata.MetaDataException.class,
																			() -> new ContentChecker().checkContent());

		assertThat(e.getMessage(), is("The dynamic entity document has persistent metadata with no table name."));
	}

	@Test
	void hasContentRecognisesContentAndImageFieldsOnly() throws Exception {
		Table scalar = new Table("Scalar", "SCALAR");
		scalar.fields.put("name", new BackupField(AttributeType.text, Sensitivity.none));
		Table content = new Table("Content", "CONTENT");
		content.fields.put("upload", new BackupField(AttributeType.content, Sensitivity.none));
		Table image = new Table("Image", "IMAGE");
		image.fields.put("photo", new BackupField(AttributeType.image, Sensitivity.none));

		assertThat(invokeHasContent(scalar), is(false));
		assertThat(invokeHasContent(content), is(true));
		assertThat(invokeHasContent(image), is(true));
	}

	@Test
	void checkContentCountsMissingAttachment() throws Exception {
		ContentChecker checker = new ContentChecker();
		@SuppressWarnings("resource")
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		when(cm.getAttachment("missing")).thenReturn(null);

		invokeCheckContent(checker, "missing", cm, "upload", "DOC", AttributeType.content, customer, false);

		assertThat(counter(checker, "missingContentCount"), is(1));
		assertThat(counter(checker, "erroneousContentCount"), is(0));
	}

	@Test
	void checkContentCountsAttributeNameMismatch() throws Exception {
		ContentChecker checker = new ContentChecker();
		@SuppressWarnings("resource")
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		when(cm.getAttachment("content-id")).thenReturn(attachment("wrongName"));

		invokeCheckContent(checker, "content-id", cm, "upload", "DOC", AttributeType.content, customer, false);

		assertThat(counter(checker, "missingContentCount"), is(0));
		assertThat(counter(checker, "erroneousContentCount"), is(1));
	}

	@Test
	void checkContentCountsNonPersistentDocument() throws Exception {
		ContentChecker checker = new ContentChecker();
		@SuppressWarnings("resource")
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(cm.getAttachment("content-id")).thenReturn(attachment("upload"));
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		when(document.getPersistent()).thenReturn(null);

		invokeCheckContent(checker, "content-id", cm, "upload", "DOC", AttributeType.content, customer, false);

		assertThat(counter(checker, "erroneousContentCount"), is(1));
	}

	@Test
	@SuppressWarnings("null")
	void checkContentCountsPersistentIdentifierMismatchForStaticDocument() throws Exception {
		ContentChecker checker = new ContentChecker();
		@SuppressWarnings("resource")
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		Document document = document(customer, "OTHER_DOC", attribute("upload", AttributeType.content));
		when(cm.getAttachment("content-id")).thenReturn(attachment("upload"));

		invokeCheckContent(checker, "content-id", cm, "upload", "DOC", AttributeType.content, customer, false);

		assertThat(counter(checker, "erroneousContentCount"), is(1));
		assertThat(document.getPersistent().getPersistentIdentifier(), is("OTHER_DOC"));
	}

	@Test
	void checkContentCountsMissingPolymorphicAttribute() throws Exception {
		ContentChecker checker = new ContentChecker();
		@SuppressWarnings("resource")
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		document(customer, "DOC");
		when(cm.getAttachment("content-id")).thenReturn(attachment("upload"));

		invokeCheckContent(checker, "content-id", cm, "upload", "DOC", AttributeType.content, customer, false);

		assertThat(counter(checker, "erroneousContentCount"), is(1));
	}

	@Test
	void checkContentCountsAttributeTypeMismatch() throws Exception {
		ContentChecker checker = new ContentChecker();
		@SuppressWarnings("resource")
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		document(customer, "DOC", attribute("upload", AttributeType.text));
		when(cm.getAttachment("content-id")).thenReturn(attachment("upload"));

		invokeCheckContent(checker, "content-id", cm, "upload", "DOC", AttributeType.content, customer, false);

		assertThat(counter(checker, "erroneousContentCount"), is(1));
	}

	@Test
	void checkContentAcceptsEmbeddedFieldNameSuffixAndSkipsPersistentIdentifierMismatch() throws Exception {
		ContentChecker checker = new ContentChecker();
		@SuppressWarnings("resource")
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		document(customer, "OTHER_DOC", attribute("upload", AttributeType.content));
		when(cm.getAttachment("content-id")).thenReturn(attachment("upload"));

		invokeCheckContent(checker, "content-id", cm, "embedded_upload", "DOC", AttributeType.content, customer, false);

		assertThat(counter(checker, "missingContentCount"), is(0));
		assertThat(counter(checker, "erroneousContentCount"), is(0));
	}

	@Test
	void checkContentCountsMissingContentDocument() throws Exception {
		ContentChecker checker = new ContentChecker();
		@SuppressWarnings("resource")
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		when(cm.getAttachment("content-id")).thenReturn(attachment("upload"));
		when(customer.getModule("admin")).thenThrow(new IllegalStateException("missing module"));

		invokeCheckContent(checker, "content-id", cm, "upload", "DOC", AttributeType.content, customer, false);

		assertThat(counter(checker, "erroneousContentCount"), is(1));
	}

	@Test
	void checkContentAcceptsMatchingDynamicDocumentContent() throws Exception {
		ContentChecker checker = new ContentChecker();
		@SuppressWarnings("resource")
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		document(customer, "OTHER_DYNAMIC_DOC", attribute("upload", AttributeType.content));
		when(cm.getAttachment("content-id")).thenReturn(attachment("upload"));

		invokeCheckContent(checker, "content-id", cm, "upload", "DYN_ENTITY", AttributeType.content, customer, true);

		assertThat(counter(checker, "missingContentCount"), is(0));
		assertThat(counter(checker, "erroneousContentCount"), is(0));
	}

	@Test
	void checkContentSwallowsContentManagerException() throws Exception {
		ContentChecker checker = new ContentChecker();
		@SuppressWarnings("resource")
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		when(cm.getAttachment("content-id")).thenThrow(new IllegalStateException("boom"));

		invokeCheckContent(checker, "content-id", cm, "upload", "DOC", AttributeType.content, customer, false);

		assertThat(counter(checker, "missingContentCount"), is(0));
		assertThat(counter(checker, "erroneousContentCount"), is(0));
	}

	@Test
	@SuppressWarnings("resource")
	void checkDynamicContentReadsJsonContentFieldsAndValidatesAttachments() throws Exception {
		ContentChecker checker = new ContentChecker();
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		Attribute upload = attribute("upload", AttributeType.content);
		Attribute photo = attribute("photo", AttributeType.image);
		Attribute name = attribute("name", AttributeType.text);
		document(customer, "OTHER_DYNAMIC_DOC", upload, photo, name);
		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(true, false);
		when(resultSet.getString(2)).thenReturn("admin");
		when(resultSet.getString(3)).thenReturn("Invoice");
		when(resultSet.getString(4)).thenReturn("{\"upload\":\"content-id\",\"photo\":\"image-id\",\"name\":\"ignored\"}");
		when(cm.getAttachment("content-id")).thenReturn(attachment("upload"));
		when(cm.getAttachment("image-id")).thenReturn(attachment("photo"));

		invokeCheckDynamicContent(checker, connection, cm, customer, "demo", "DYN_ENTITY");

		org.mockito.Mockito.verify(statement).execute("select bizId, moduleName, documentName, fields from DYN_ENTITY where bizCustomer = 'demo'");
		assertThat(counter(checker, "missingContentCount"), is(0));
		assertThat(counter(checker, "erroneousContentCount"), is(0));
	}

	@Test
	@SuppressWarnings("resource")
	void checkDynamicContentSkipsRowWithNullJsonFields() throws Exception {
		ContentChecker checker = new ContentChecker();
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		document(customer, "OTHER_DYNAMIC_DOC", attribute("upload", AttributeType.content), attribute("photo", AttributeType.image));
		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(true, false);
		when(resultSet.getString(2)).thenReturn("admin");
		when(resultSet.getString(3)).thenReturn("Invoice");
		when(resultSet.getString(4)).thenReturn("null");

		invokeCheckDynamicContent(checker, connection, cm, customer, "demo", "DYN_ENTITY");

		org.mockito.Mockito.verifyNoInteractions(cm);
		assertThat(counter(checker, "missingContentCount"), is(0));
		assertThat(counter(checker, "erroneousContentCount"), is(0));
	}

	@Test
	@SuppressWarnings("resource")
	void checkDynamicContentOmitsCustomerFilterWhenSingleCustomerConfigured() throws Exception {
		ContentChecker checker = new ContentChecker();
		UtilImpl.CUSTOMER = "demo";
		Connection connection = mock(Connection.class);
		Statement statement = mock(Statement.class);
		ResultSet resultSet = mock(ResultSet.class);
		ContentManager cm = mock(ContentManager.class);
		Customer customer = mock(Customer.class);
		when(connection.createStatement()).thenReturn(statement);
		when(statement.getResultSet()).thenReturn(resultSet);
		when(resultSet.next()).thenReturn(false);

		invokeCheckDynamicContent(checker, connection, cm, customer, "demo", "DYN_ENTITY");

		org.mockito.Mockito.verify(statement).execute("select bizId, moduleName, documentName, fields from DYN_ENTITY");
	}

	@Test
	void bogusStaticContentReferenceReturnsFirstTableAndBizId() throws Exception {
		ContentChecker checker = new ContentChecker();
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		Table scalar = new Table("Scalar", "SCALAR");
		scalar.fields.put("name", new BackupField(AttributeType.text, Sensitivity.none));
		Table document = new Table("Document", "DOC");
		document.fields.put("upload", new BackupField(AttributeType.content, Sensitivity.none));
		document.fields.put("photo", new BackupField(AttributeType.image, Sensitivity.none));
		setTablesForAllCustomers(checker, List.of(scalar, document));
		when(persistence.newSQL("select bizId from DOC where upload = :contentId or photo = :contentId")).thenReturn(sql);
		when(sql.putParameter("contentId", "content-id", false)).thenReturn(sql);
		when(sql.scalarResult(String.class)).thenReturn("row-id");

		String result = invokeBogusStaticContentReference(checker, "content-id");

		assertThat(result, is("Document#row-id"));
	}

	@Test
	void bogusStaticContentReferenceReturnsNullWhenNoRowsReferenceContent() throws Exception {
		ContentChecker checker = new ContentChecker();
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		Table document = new Table("Document", "DOC");
		document.fields.put("upload", new BackupField(AttributeType.content, Sensitivity.none));
		setTablesForAllCustomers(checker, List.of(document));
		when(persistence.newSQL("select bizId from DOC where upload = :contentId")).thenReturn(sql);
		when(sql.putParameter("contentId", "content-id", false)).thenReturn(sql);
		when(sql.scalarResult(String.class)).thenReturn(null);

		String result = invokeBogusStaticContentReference(checker, "content-id");

		assertThat(result, is((String) null));
	}

	@Test
	void bogusDynamicContentReferenceReturnsDynamicEntityReferenceForContentAttribute() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute upload = attribute("upload", AttributeType.content);
		ProvidedRepositoryFactory.set(repository);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like")).thenReturn(sql);
		when(sql.putParameter("like", "%\":\"content-id\"%", false)).thenReturn(sql);
		when(sql.tupleResults()).thenReturn(List.<Object[]> of(new Object[] {"dyn-id", "demo", "admin", "Invoice", "{\"upload\":\"content-id\"}"}));
		when(repository.getCustomer("demo")).thenReturn(customer);
		when(repository.getModule(customer, "admin")).thenReturn(module);
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "upload")).thenReturn(upload);

		String result = invokeBogusDynamicContentReference("content-id", "DYN_ENTITY");

		assertThat(result, is("DYN_ENTITY#dyn-id"));
	}

	@Test
	void bogusDynamicContentReferenceReturnsNullWhenMatchedAttributeIsNotContent() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute text = attribute("name", AttributeType.text);
		ProvidedRepositoryFactory.set(repository);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like")).thenReturn(sql);
		when(sql.putParameter("like", "%\":\"content-id\"%", false)).thenReturn(sql);
		when(sql.tupleResults()).thenReturn(List.<Object[]> of(new Object[] {"dyn-id", "demo", "admin", "Invoice", "{\"name\":\"content-id\"}"}));
		when(repository.getCustomer("demo")).thenReturn(customer);
		when(repository.getModule(customer, "admin")).thenReturn(module);
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "name")).thenReturn(text);

		String result = invokeBogusDynamicContentReference("content-id", "DYN_ENTITY");

		assertThat(result, is((String) null));
	}

	@Test
	void bogusDynamicContentReferenceReturnsDynamicEntityReferenceForImageAttribute() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute image = attribute("photo", AttributeType.image);
		ProvidedRepositoryFactory.set(repository);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like")).thenReturn(sql);
		when(sql.putParameter("like", "%\":\"content-id\"%", false)).thenReturn(sql);
		when(sql.tupleResults()).thenReturn(List.<Object[]> of(new Object[] {"dyn-id", "demo", "admin", "Invoice", "{\"photo\":\"content-id\"}"}));
		when(repository.getCustomer("demo")).thenReturn(customer);
		when(repository.getModule(customer, "admin")).thenReturn(module);
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "photo")).thenReturn(image);

		String result = invokeBogusDynamicContentReference("content-id", "DYN_ENTITY");

		assertThat(result, is("DYN_ENTITY#dyn-id"));
	}

	@Test
	void bogusDynamicContentReferenceReturnsNullWhenCustomerCannotBeResolved() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like")).thenReturn(sql);
		when(sql.putParameter("like", "%\":\"content-id\"%", false)).thenReturn(sql);
		when(sql.tupleResults()).thenReturn(List.<Object[]> of(new Object[] {"dyn-id", "demo", "admin", "Invoice", "{\"upload\":\"content-id\"}"}));
		when(repository.getCustomer("demo")).thenReturn(null);

		String result = invokeBogusDynamicContentReference("content-id", "DYN_ENTITY");

		assertThat(result, is((String) null));
	}

	@Test
	void bogusDynamicContentReferenceReturnsNullWhenModuleCannotBeResolved() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		ProvidedRepositoryFactory.set(repository);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like")).thenReturn(sql);
		when(sql.putParameter("like", "%\":\"content-id\"%", false)).thenReturn(sql);
		when(sql.tupleResults()).thenReturn(List.<Object[]> of(new Object[] {"dyn-id", "demo", "admin", "Invoice", "{\"upload\":\"content-id\"}"}));
		when(repository.getCustomer("demo")).thenReturn(customer);
		when(repository.getModule(customer, "admin")).thenReturn(null);

		String result = invokeBogusDynamicContentReference("content-id", "DYN_ENTITY");

		assertThat(result, is((String) null));
	}

	@Test
	void bogusDynamicContentReferenceReturnsNullWhenDocumentCannotBeResolved() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		ProvidedRepositoryFactory.set(repository);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like")).thenReturn(sql);
		when(sql.putParameter("like", "%\":\"content-id\"%", false)).thenReturn(sql);
		when(sql.tupleResults()).thenReturn(List.<Object[]> of(new Object[] {"dyn-id", "demo", "admin", "Invoice", "{\"upload\":\"content-id\"}"}));
		when(repository.getCustomer("demo")).thenReturn(customer);
		when(repository.getModule(customer, "admin")).thenReturn(module);
		when(module.getDocument(customer, "Invoice")).thenReturn(null);

		String result = invokeBogusDynamicContentReference("content-id", "DYN_ENTITY");

		assertThat(result, is((String) null));
	}

	@Test
	void bogusDynamicContentReferenceReturnsNullWhenPolymorphicAttributeCannotBeResolved() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		ProvidedRepositoryFactory.set(repository);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like")).thenReturn(sql);
		when(sql.putParameter("like", "%\":\"content-id\"%", false)).thenReturn(sql);
		when(sql.tupleResults()).thenReturn(List.<Object[]> of(new Object[] {"dyn-id", "demo", "admin", "Invoice", "{\"upload\":\"content-id\"}"}));
		when(repository.getCustomer("demo")).thenReturn(customer);
		when(repository.getModule(customer, "admin")).thenReturn(module);
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		when(document.getPolymorphicAttribute(customer, "upload")).thenReturn(null);

		String result = invokeBogusDynamicContentReference("content-id", "DYN_ENTITY");

		assertThat(result, is((String) null));
	}

	@Test
	void bogusDynamicContentReferenceReturnsNullWhenNoRowsReferenceContent() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like")).thenReturn(sql);
		when(sql.putParameter("like", "%\":\"content-id\"%", false)).thenReturn(sql);
		when(sql.tupleResults()).thenReturn(List.of());

		String result = invokeBogusDynamicContentReference("content-id", "DYN_ENTITY");

		assertThat(result, is((String) null));
	}

	@Test
	void bogusDynamicContentReferenceReturnsNullWhenAttributeNameCannotBeParsed() throws Exception {
		AbstractPersistence persistence = bindMockPersistence();
		SQL sql = mock(SQL.class);
		ProvidedRepository repository = mock(ProvidedRepository.class);
		ProvidedRepositoryFactory.set(repository);
		when(persistence.newSQL("select bizId, bizCustomer, moduleName, documentName, fields from DYN_ENTITY where fields like :like")).thenReturn(sql);
		when(sql.putParameter("like", "%\":\"content-id\"%", false)).thenReturn(sql);
		when(sql.tupleResults()).thenReturn(List.<Object[]> of(new Object[] {"dyn-id", "demo", "admin", "Invoice", "\":\"content-id\""}));

		String result = invokeBogusDynamicContentReference("content-id", "DYN_ENTITY");

		assertThat(result, is((String) null));
	}

	private static void invokeCheckContent(ContentChecker checker,
											String contentId,
											ContentManager cm,
											String fieldName,
											String persistentIdentifier,
											AttributeType attributeType,
											Customer customer,
											boolean dynamicDocument)
	throws Exception {
		Method method = ContentChecker.class.getDeclaredMethod("checkContent",
				String.class,
				ContentManager.class,
				String.class,
				String.class,
				AttributeType.class,
				Customer.class,
				boolean.class);
		method.setAccessible(true);
		method.invoke(checker, contentId, cm, fieldName, persistentIdentifier, attributeType, customer, Boolean.valueOf(dynamicDocument));
	}

	private static void invokeCheckDynamicContent(ContentChecker checker,
													Connection connection,
													ContentManager cm,
													Customer customer,
													String customerName,
													String dynamicEntityPersistentIdentifier)
	throws Exception {
		Method method = ContentChecker.class.getDeclaredMethod("checkDynamicContent",
				Connection.class,
				ContentManager.class,
				Customer.class,
				String.class,
				String.class);
		method.setAccessible(true);
		method.invoke(checker, connection, cm, customer, customerName, dynamicEntityPersistentIdentifier);
	}

	private static String invokeBogusStaticContentReference(ContentChecker checker, String contentId) throws Exception {
		Method method = ContentChecker.class.getDeclaredMethod("bogusStaticContentReference", String.class);
		method.setAccessible(true);
		return (String) method.invoke(checker, contentId);
	}

	private static String invokeBogusDynamicContentReference(String contentId, String dynamicEntityPersistentIdentifier) throws Exception {
		Method method = ContentChecker.class.getDeclaredMethod("bogusDynamicContentReference", String.class, String.class);
		method.setAccessible(true);
		return (String) method.invoke(null, contentId, dynamicEntityPersistentIdentifier);
	}

	private static boolean invokeHasContent(Table table) throws Exception {
		Method method = ContentChecker.class.getDeclaredMethod("hasContent", Table.class);
		method.setAccessible(true);
		return ((Boolean) method.invoke(null, table)).booleanValue();
	}

	private static int counter(ContentChecker checker, String fieldName) throws Exception {
		Field field = ContentChecker.class.getDeclaredField(fieldName);
		field.setAccessible(true);
		return ((Integer) field.get(checker)).intValue();
	}

	private static void setTablesForAllCustomers(ContentChecker checker, Collection<Table> tables) throws Exception {
		Field field = ContentChecker.class.getDeclaredField("tablesForAllCustomers");
		field.setAccessible(true);
		field.set(checker, tables);
	}

	private static AttachmentContent attachment(String attributeName) {
		return new AttachmentContent("demo", "admin", "Invoice", null, "user", "biz-id", attributeName);
	}

	private static Document document(Customer customer, String persistentIdentifier, Attribute... attributes) {
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Persistent persistent = new Persistent();
		persistent.setName(persistentIdentifier);
		when(customer.getModule("admin")).thenReturn(module);
		when(module.getDocument(customer, "Invoice")).thenReturn(document);
		when(document.getPersistent()).thenReturn(persistent);
		doReturn(List.<Attribute> of(attributes)).when(document).getAllAttributes(customer);
		for (Attribute attribute : attributes) {
			when(document.getPolymorphicAttribute(customer, attribute.getName())).thenReturn(attribute);
		}
		return document;
	}

	private static Attribute attribute(String name, AttributeType attributeType) {
		Attribute attribute = mock(Attribute.class);
		when(attribute.getName()).thenReturn(name);
		when(attribute.getAttributeType()).thenReturn(attributeType);
		return attribute;
	}

	private static AbstractPersistence bindMockPersistence() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
		return persistence;
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
