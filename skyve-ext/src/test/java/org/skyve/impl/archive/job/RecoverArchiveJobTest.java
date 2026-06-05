package org.skyve.impl.archive.job;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.archive.support.CorruptArchiveError;
import org.skyve.archive.support.CorruptArchiveError.Resolution;
import org.skyve.impl.util.UtilImpl.ArchiveConfig.ArchiveDocConfig;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

@SuppressWarnings("static-method")
class RecoverArchiveJobTest {
	@TempDir
	Path tempDir;

	@Test
	void undoSoftDeleteSQLFormatsArchiveColumns() throws Exception {
		RecoverArchiveJob job = new RecoverArchiveJob();

		assertEquals("""
				update ADM_Audit
				  set  archiveTimestamp = null
				      ,archiveFilename = null
				where archiveFilename = :filename
				""", invokeUndoSoftDeleteSQL(job, "ADM_Audit"));
	}

	@Test
	void chooseDestinationPathUsesFirstAvailableCorruptSuffix() throws Exception {
		RecoverArchiveJob job = new RecoverArchiveJob();
		Path archive = tempDir.resolve("audit.archive");
		Files.createFile(archive);
		Files.createFile(tempDir.resolve("audit.archive.0.corrupt"));

		Path destination = invokeChooseDestinationPath(job, archive);

		assertEquals(tempDir.resolve("audit.archive.1.corrupt"), destination);
	}

	@Test
	void chooseDestinationPathThrowsWhenAllSuffixesExist() throws Exception {
		RecoverArchiveJob job = new RecoverArchiveJob();
		Path archive = tempDir.resolve("audit.archive");
		Files.createFile(archive);
		for (int i = 0; i < 100; i++) {
			Files.createFile(tempDir.resolve("audit.archive." + i + ".corrupt"));
		}

		InvocationTargetException thrown = assertThrows(InvocationTargetException.class,
				() -> chooseDestinationPathMethod().invoke(job, archive));

		assertEquals("Unable to find destination path for corrupt archive: " + archive, thrown.getCause().getMessage());
	}

	@Test
	void renameArchiveMovesFileToChosenDestination() throws Exception {
		RecoverArchiveJob job = new RecoverArchiveJob();
		Path archive = tempDir.resolve("audit.archive");
		Files.writeString(archive, "corrupt");

		invokeRenameArchive(job, archive);

		assertFalse(Files.exists(archive));
		assertEquals("corrupt", Files.readString(tempDir.resolve("audit.archive.0.corrupt")));
		assertTrue(job.getLog().get(0).contains("Renaming corrupted archive file"));
	}

	@Test
	void assertFileExistsAcceptsRegularFileAndRejectsMissingPath() throws Exception {
		RecoverArchiveJob job = new RecoverArchiveJob();
		ArchiveDocConfig config = new ArchiveDocConfig("admin", "Audit", "audit", 30);
		Path archive = tempDir.resolve("audit.archive");
		Files.createFile(archive);

		invokeAssertFileExists(job, config, archive);
		InvocationTargetException thrown = assertThrows(InvocationTargetException.class,
				() -> assertFileExistsMethod().invoke(job, config, tempDir.resolve("missing.archive")));

		assertTrue(thrown.getCause().getMessage().contains("Corrupt archive path"));
		assertEquals(thrown.getCause().getMessage(), job.getLog().get(0));
	}

	@Test
	void recoveryExceptionConstructorsPreserveMessageAndCause() throws Exception {
		Throwable cause = new IllegalStateException("root");
		RuntimeException messageOnly = newRecoveryException("message");
		RuntimeException withCause = newRecoveryException("message", cause);

		assertEquals("message", messageOnly.getMessage());
		assertEquals("message", withCause.getMessage());
		assertSame(cause, withCause.getCause());
	}

	@Test
	void listUnresolvedErrorsBuildsResolutionFilter() throws Exception {
		RecoverArchiveJob job = new RecoverArchiveJob();
		Persistence persistence = mock(Persistence.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		CorruptArchiveError error = mock(CorruptArchiveError.class);
		when(persistence.newDocumentQuery(CorruptArchiveError.MODULE_NAME, CorruptArchiveError.DOCUMENT_NAME)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(filter.addEquals(CorruptArchiveError.resolutionPropertyName, Resolution.unresolved)).thenReturn(filter);
		when(query.beanResults()).thenReturn(List.of(error));
		setPersistence(job, persistence);

		assertEquals(List.of(error), invokeListUnresolvedErrors(job));
		verify(filter).addEquals(CorruptArchiveError.resolutionPropertyName, Resolution.unresolved);
	}

	private static String invokeUndoSoftDeleteSQL(RecoverArchiveJob job, String tableName) throws Exception {
		Method method = RecoverArchiveJob.class.getDeclaredMethod("undoSoftDeleteSQL", String.class);
		method.setAccessible(true);
		return (String) method.invoke(job, tableName);
	}

	private static Path invokeChooseDestinationPath(RecoverArchiveJob job, Path archive) throws Exception {
		return (Path) chooseDestinationPathMethod().invoke(job, archive);
	}

	private static Method chooseDestinationPathMethod() throws Exception {
		Method method = RecoverArchiveJob.class.getDeclaredMethod("chooseDestinationPath", Path.class);
		method.setAccessible(true);
		return method;
	}

	private static void invokeRenameArchive(RecoverArchiveJob job, Path archive) throws Exception {
		Method method = RecoverArchiveJob.class.getDeclaredMethod("renameArchive", Path.class);
		method.setAccessible(true);
		method.invoke(job, archive);
	}

	private static void invokeAssertFileExists(RecoverArchiveJob job, ArchiveDocConfig config, Path archive) throws Exception {
		assertFileExistsMethod().invoke(job, config, archive);
	}

	private static Method assertFileExistsMethod() throws Exception {
		Method method = RecoverArchiveJob.class.getDeclaredMethod("assertFileExists", ArchiveDocConfig.class, Path.class);
		method.setAccessible(true);
		return method;
	}

	private static RuntimeException newRecoveryException(String message) throws Exception {
		Class<?> exceptionClass = recoveryExceptionClass();
		Constructor<?> constructor = exceptionClass.getDeclaredConstructor(String.class);
		constructor.setAccessible(true);
		return (RuntimeException) constructor.newInstance(message);
	}

	private static RuntimeException newRecoveryException(String message, Throwable cause) throws Exception {
		Class<?> exceptionClass = recoveryExceptionClass();
		Constructor<?> constructor = exceptionClass.getDeclaredConstructor(String.class, Throwable.class);
		constructor.setAccessible(true);
		return (RuntimeException) constructor.newInstance(message, cause);
	}

	private static Class<?> recoveryExceptionClass() throws Exception {
		return Class.forName("org.skyve.impl.archive.job.RecoverArchiveJob$RecoveryException");
	}

	@SuppressWarnings("unchecked")
	private static List<CorruptArchiveError> invokeListUnresolvedErrors(RecoverArchiveJob job) throws Exception {
		Method method = RecoverArchiveJob.class.getDeclaredMethod("listUnresolvedErrors");
		method.setAccessible(true);
		return (List<CorruptArchiveError>) method.invoke(job);
	}

	private static void setPersistence(RecoverArchiveJob job, Persistence persistence) throws Exception {
		Field field = RecoverArchiveJob.class.getDeclaredField("persistence");
		field.setAccessible(true);
		field.set(job, persistence);
	}
}
