package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class DDLTest {
	@TempDir
	Path tempDir;

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
}
