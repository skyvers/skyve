package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Field;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.impl.persistence.AbstractPersistence;

@SuppressWarnings("static-method")
class DDLTest {
	@TempDir
	Path tempDir;

	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void dropAndCreateReadScriptFilesWithoutExecuting() throws Exception {
		Path dropScript = tempDir.resolve("drop.sql");
		Path createScript = tempDir.resolve("create.sql");
		Files.writeString(dropScript, "drop table TEST;\ndrop index IDX_TEST\n");
		Files.writeString(createScript, "create table TEST (bizId varchar(36));\ncreate index IDX_TEST on TEST (bizId)\n");

		assertEquals(List.of("drop table TEST", "drop index IDX_TEST"), DDL.drop(dropScript.toFile(), false));
		assertEquals(List.of("create table TEST (bizId varchar(36))", "create index IDX_TEST on TEST (bizId)"),
				DDL.create(createScript.toFile(), false));
	}

	@Test
	void dropCreateAndSyncGenerateDDLWhenScriptsAreNotProvided() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		doAnswer(invocation -> {
			@SuppressWarnings("unchecked")
			List<String> drops = (List<String>) invocation.getArgument(0);
			@SuppressWarnings("unchecked")
			List<String> creates = (List<String>) invocation.getArgument(1);
			@SuppressWarnings("unchecked")
			List<String> updates = (List<String>) invocation.getArgument(2);
			if (drops != null) {
				drops.add("drop generated");
			}
			if (creates != null) {
				creates.add("create generated");
			}
			if (updates != null) {
				updates.add("sync generated");
			}
			return null;
		}).when(persistence).generateDDL(any(), any(), any());
		bindPersistenceToThread(persistence);

		assertEquals(List.of("drop generated"), DDL.drop(null, false));
		assertEquals(List.of("create generated"), DDL.create(null, false));
		assertEquals(List.of("sync generated"), DDL.sync(false));
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		@SuppressWarnings("unchecked")
		ThreadLocal<AbstractPersistence> threadLocal = (ThreadLocal<AbstractPersistence>) field.get(null);
		threadLocal.remove();
	}
}
