package org.skyve.impl.archive.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.io.File;
import java.nio.file.Path;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.objenesis.ObjenesisStd;
import org.skyve.archive.support.ArchiveableBean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Interface;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;

@SuppressWarnings("static-method")
class ExportDocumentsToArchiveJobTest {
	private static final String ORIGINAL_CONTENT_DIRECTORY = UtilImpl.CONTENT_DIRECTORY;

	@TempDir
	Path tempDir;

	@AfterEach
	void restoreContentDirectory() {
		UtilImpl.CONTENT_DIRECTORY = ORIGINAL_CONTENT_DIRECTORY;
	}

	@Test
	void validateExportedTypeAcceptsDocumentsImplementingArchiveableBean() {
		ExportDocumentsToArchiveJob job = newJob();
		Document document = documentWithInterfaces(interfaceNamed(ArchiveableBean.class.getName()));

		job.validateExportedType(document);
	}

	@Test
	void validateExportedTypeRejectsDocumentsWithoutArchiveableBeanInterface() {
		ExportDocumentsToArchiveJob job = newJob();
		Document document = documentWithInterfaces(interfaceNamed("example.Other"));
		when(document.toString()).thenReturn("sales.Order");

		IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class,
				() -> job.validateExportedType(document));

		assertEquals("Document type sales.Order does not implement interface "
				+ ArchiveableBean.class.getName()
				+ " and cannot be archived", thrown.getMessage());
		assertEquals(List.of(thrown.getMessage()), job.getLog());
	}

	@Test
	void jobSubstanceCreatesSoftDeleteUpdateStatement() throws Exception {
		ExportDocumentsToArchiveJob job = newJob();
		Object substance = newJobSubstance(job, persistentDocument("orders_table"),
											new ArchiveDocConfig("sales", "Order", "orders", 30));

		Method method = substance.getClass().getDeclaredMethod("createUpdateStatement");
		method.setAccessible(true);

		assertEquals("""
				update orders_table
				  set  archiveTimestamp = :time
				      ,archiveFilename  = :file
				where bizId = :id""", method.invoke(substance));
	}

	@Test
	void jobSubstanceSoftDeleteDocumentBindsArchiveColumnsAndExecutes() throws Exception {
		ExportDocumentsToArchiveJob job = newJob();
		Persistence persistence = mock(Persistence.class);
		SQL sql = mock(SQL.class);
		when(persistence.newSQL(any(String.class))).thenReturn(sql);
		when(sql.putParameter(eq("time"), any(Timestamp.class))).thenReturn(sql);
		when(sql.putParameter("file", "orders-2026-06-05.archive", false)).thenReturn(sql);
		when(sql.putParameter("id", "order-123", false)).thenReturn(sql);
		setField(job, "persistence", persistence);
		Object substance = newJobSubstance(job, persistentDocument("orders_table"),
											new ArchiveDocConfig("sales", "Order", "orders", 30));
		Method method = substance.getClass()
								.getDeclaredMethod("softDeleteDocument", DynamicBean.class, Timestamp.class, String.class);
		method.setAccessible(true);
		DynamicBean bean = mock(DynamicBean.class);
		when(bean.getBizId()).thenReturn("order-123");
		Timestamp timestamp = new Timestamp();

		method.invoke(substance, bean, timestamp, "orders-2026-06-05.archive");

		verify(persistence).newSQL("""
				update orders_table
				  set  archiveTimestamp = :time
				      ,archiveFilename  = :file
				where bizId = :id""");
		verify(sql).putParameter("time", timestamp);
		verify(sql).putParameter("file", "orders-2026-06-05.archive", false);
		verify(sql).putParameter("id", "order-123", false);
		verify(sql).execute();
	}

	@Test
	void jobSubstanceLogOnceSuppressesConsecutiveDuplicateMessages() throws Exception {
		ExportDocumentsToArchiveJob job = newJob();
		Object substance = newJobSubstance(job, persistentDocument("orders_table"),
											new ArchiveDocConfig("sales", "Order", "orders", 30));
		Method method = substance.getClass()
								.getDeclaredMethod("logOnce", String.class);
		method.setAccessible(true);

		method.invoke(substance, "Exporting documents");
		method.invoke(substance, "Exporting documents");
		method.invoke(substance, "Done");

		assertEquals(List.of("Exporting documents", "Done"), job.getLog());
	}

	@Test
	void jobSubstanceArchiveFileUsesLowerCaseDocumentNameAndCurrentDate() throws Exception {
		UtilImpl.CONTENT_DIRECTORY = tempDir.toString();
		ExportDocumentsToArchiveJob job = newJob();
		Object substance = newJobSubstance(job, persistentDocument("orders_table"),
											new ArchiveDocConfig("sales", "Order", "orders", 30));
		Method method = substance.getClass()
								.getDeclaredMethod("getArchiveFile");
		method.setAccessible(true);
		String datePart = DateTimeFormatter.ofPattern("yyyy-MM-dd")
											.withZone(ZoneId.systemDefault())
											.format(java.time.Instant.now());

		File result = (File) method.invoke(substance);

		assertEquals("order-" + datePart + ".archive", result.getName());
		assertEquals(tempDir.resolve("archive")
							.resolve("orders")
							.toFile(), result.getParentFile());
	}

	@Test
	void jobSubstanceGetDocumentsToExportBuildsNullArchiveTimestampQuery() throws Exception {
		ExportDocumentsToArchiveJob job = newJob();
		Persistence persistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		DynamicBean bean = mock(DynamicBean.class);
		Document document = persistentDocument("orders_table");
		when(persistence.newDocumentQuery(document)).thenReturn(query);
		when(query.setMaxResults(7)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(filter.addNull(ArchiveableBean.archiveTimestampPropertyName)).thenReturn(filter);
		doReturn(List.of(bean)).when(query)
								.projectedResults();
		setField(job, "persistence", persistence);
		Object substance = newJobSubstance(job, document, new ArchiveDocConfig("sales", "Order", "orders", 30));
		Method method = substance.getClass()
								.getDeclaredMethod("getDocumentsToExport", int.class);
		method.setAccessible(true);

		assertEquals(List.of(bean), method.invoke(substance, Integer.valueOf(7)));
		verify(filter).addNull(ArchiveableBean.archiveTimestampPropertyName);
	}

	@Test
	void jobSubstanceGetDeletableBeansBuildsArchivedBeforeCutoffQuery() throws Exception {
		ExportDocumentsToArchiveJob job = newJob();
		setField(job, "batchSize", Integer.valueOf(5));
		Persistence persistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		Document document = persistentDocument("orders_table");
		when(persistence.newDocumentQuery(document)).thenReturn(query);
		when(query.setMaxResults(5)).thenReturn(query);
		when(query.addBoundProjection("bizId")).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(filter.addLessThanOrEqualTo(eq(ArchiveableBean.archiveTimestampPropertyName), any(Timestamp.class))).thenReturn(filter);
		when(query.scalarResults(String.class)).thenReturn(List.of("order-1"));
		setField(job, "persistence", persistence);
		Object substance = newJobSubstance(job, document, new ArchiveDocConfig("sales", "Order", "orders", 30));
		Method method = substance.getClass()
								.getDeclaredMethod("getDeletableBeans");
		method.setAccessible(true);

		assertEquals(List.of("order-1"), method.invoke(substance));
		verify(query).addBoundProjection("bizId");
		verify(filter).addLessThanOrEqualTo(eq(ArchiveableBean.archiveTimestampPropertyName), any(Timestamp.class));
	}

	private static ExportDocumentsToArchiveJob newJob() {
		ExportDocumentsToArchiveJob job = new ObjenesisStd().newInstance(ExportDocumentsToArchiveJob.class);
		try {
			setField(job, "logger", LogManager.getLogger(ExportDocumentsToArchiveJob.class));
			setSuperclassField(job, "log", new ArrayList<>());
		}
		catch (Exception e) {
			throw new IllegalStateException("Could not initialise allocated export job", e);
		}
		return job;
	}

	private static Object newJobSubstance(ExportDocumentsToArchiveJob job, Document document, ArchiveDocConfig config)
	throws Exception {
		Class<?> clazz = Class.forName("org.skyve.impl.archive.job.ExportDocumentsToArchiveJob$JobSubstance");
		Constructor<?> constructor = clazz.getDeclaredConstructor(ExportDocumentsToArchiveJob.class, Document.class,
																	ArchiveDocConfig.class);
		constructor.setAccessible(true);
		return constructor.newInstance(job, document, config);
	}

	private static Document persistentDocument(String tableName) {
		Document document = mock(Document.class);
		Persistent persistent = new Persistent();
		persistent.setName(tableName);
		when(document.getPersistent()).thenReturn(persistent);
		when(document.getName()).thenReturn("Order");
		return document;
	}

	private static Document documentWithInterfaces(Interface... interfaces) {
		Document document = mock(Document.class);
		doReturn(List.of(interfaces)).when(document).getInterfaces();
		return document;
	}

	private static Interface interfaceNamed(String interfaceName) {
		Interface result = mock(Interface.class);
		when(result.getInterfaceName()).thenReturn(interfaceName);
		return result;
	}

	private static void setField(Object target, String name, Object value) throws Exception {
		Field field = target.getClass()
							.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}

	private static void setSuperclassField(Object target, String name, Object value) throws Exception {
		Class<?> clazz = target.getClass();
		while (clazz != null) {
			try {
				Field field = clazz.getDeclaredField(name);
				field.setAccessible(true);
				field.set(target, value);
				return;
			}
			catch (@SuppressWarnings("unused") NoSuchFieldException e) {
				clazz = clazz.getSuperclass();
			}
		}
		throw new NoSuchFieldException(name);
	}
}
