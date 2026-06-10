package org.skyve.impl.content;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.PrintStream;
import java.lang.reflect.Field;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Date;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.skyve.content.AttachmentContent;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;

@SuppressWarnings({"static-method", "boxing"})
public class AbstractContentManagerFileSystemTest {
	private static final String CONTENT_ID = "ABCDE12FGH34IJK56LMNOP";

	private boolean originalContentFileSuffixes;

	@Rule
	public TemporaryFolder temp = new TemporaryFolder();

	@Before
	public void setUp() {
		originalContentFileSuffixes = UtilImpl.CONTENT_FILE_SUFFIXES;
		UtilImpl.CONTENT_FILE_SUFFIXES = true;
	}

	@After
	public void tearDown() throws Exception {
		UtilImpl.CONTENT_FILE_SUFFIXES = originalContentFileSuffixes;
		unbindPersistenceFromThread();
	}

	@Test
	public void appendBalancedFolderPathFromContentIdUsesExpectedSegments() {
		StringBuilder path = new StringBuilder();

		AbstractContentManager.appendBalancedFolderPathFromContentId(CONTENT_ID, path);

		assertThat(path.toString(), is("12/34/56/abcde12fgh34ijk56lmnop/"));
	}

	@Test
	public void mainRequiresContentIdArgument() {
		assertThrows(IllegalArgumentException.class, () -> AbstractContentManager.main(new String[0]));
	}

	@Test
	public void mainPrintsBalancedFolderPathForContentId() {
		PrintStream originalOut = System.out;
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		try {
			System.setOut(new PrintStream(out, true, StandardCharsets.UTF_8));

			AbstractContentManager.main(new String[] { CONTENT_ID });

			assertEquals("12/34/56/abcde12fgh34ijk56lmnop/" + System.lineSeparator(), out.toString(StandardCharsets.UTF_8));
		}
		finally {
			System.setOut(originalOut);
		}
	}

	@Test
	public void writeAndReadContentFilesWithDerivedSuffix() throws Exception {
		AttachmentContent attachment = attachment("report.PDF", "application/pdf");
		StringBuilder root = rootBuilder();

		AbstractContentManager.writeContentFiles(root, attachment, new ByteArrayInputStream("pdf".getBytes(StandardCharsets.UTF_8)), false);

		Path contentDir = balancedPath();
		assertThat(Files.exists(contentDir.resolve(AbstractContentManager.META_JSON)), is(true));
		assertThat(Files.readString(contentDir.resolve("content.pdf")), is("pdf"));

		AttachmentContent read = AbstractContentManager.getFromFileSystem(rootBuilder(), CONTENT_ID, false);
		assertNotNull(read);
		assertThat(read.getContentId(), is(CONTENT_ID));
		assertThat(read.getFileName(), is("report.PDF"));
		assertThat(read.getContentType(), is("application/pdf"));
		assertThat(read.getBizCustomer(), is("customer"));
		assertThat(read.getBizModule(), is("module"));
		assertThat(read.getBizDocument(), is("document"));
		assertThat(read.getBizDataGroupId(), is("group"));
		assertThat(read.getBizUserId(), is("user"));
		assertThat(read.getBizId(), is("biz"));
		assertThat(read.getAttributeName(), is("attribute"));
		assertThat(read.getMarkup(), is("<svg />"));
		assertNotNull(read.getLastModified());
	}

	@Test
	public void writeAndReadContentFilesWithoutSuffix() throws Exception {
		AttachmentContent attachment = attachment("photo.jpg", "image/jpeg");
		StringBuilder root = rootBuilder();

		AbstractContentManager.writeContentFiles(root, attachment, new ByteArrayInputStream("jpeg".getBytes(StandardCharsets.UTF_8)), true);

		Path contentDir = balancedPath();
		assertThat(Files.readString(contentDir.resolve(AbstractContentManager.CONTENT)), is("jpeg"));
		assertNotNull(AbstractContentManager.getFromFileSystem(rootBuilder(), CONTENT_ID, true));
		assertNull(AbstractContentManager.getFromFileSystem(rootBuilder(), CONTENT_ID, false));
	}

	@Test
	public void writeContentFilesOverwritesExistingContentAndRemovesOldFile() throws Exception {
		AttachmentContent attachment = attachment("report.pdf", "application/pdf");
		AbstractContentManager.writeContentFiles(rootBuilder(), attachment, new ByteArrayInputStream("old".getBytes(StandardCharsets.UTF_8)), false);

		AbstractContentManager.writeContentFiles(rootBuilder(), attachment, new ByteArrayInputStream("new".getBytes(StandardCharsets.UTF_8)), false);

		Path contentDir = balancedPath();
		assertThat(Files.readString(contentDir.resolve("content.pdf")), is("new"));
		assertThat(Files.exists(contentDir.resolve("content.pdf_old")), is(false));
		assertThat(Files.exists(contentDir.resolve(AbstractContentManager.META_JSON + "_old")), is(false));
	}

	@Test
	public void writeContentFilesCanDeriveSuffixFromContentTypeOrSuppressGlobalSuffixes() throws Exception {
		AttachmentContent attachment = attachment("content", "application/pdf");

		AbstractContentManager.writeContentFiles(rootBuilder(), attachment, new ByteArrayInputStream("pdf".getBytes(StandardCharsets.UTF_8)), false);
		assertThat(Files.readString(balancedPath().resolve("content.pdf")), is("pdf"));

		UtilImpl.CONTENT_FILE_SUFFIXES = false;
		AbstractContentManager.writeContentFiles(rootBuilder(), attachment, new ByteArrayInputStream("plain".getBytes(StandardCharsets.UTF_8)), false);
		assertThat(Files.readString(balancedPath().resolve(AbstractContentManager.CONTENT)), is("plain"));
	}

	@Test
	public void writeExternalContentFileReadsExternalFilePath() throws Exception {
		Path external = temp.getRoot().toPath().resolve("outside.txt");
		Files.writeString(external, "external");
		AttachmentContent attachment = attachment("ignored.txt", "text/plain");
		attachment.setExternalAbsoluteFilePath(external.toString());

		AbstractContentManager.writeExternalContentFile(rootBuilder(), attachment);

		AttachmentContent read = AbstractContentManager.getFromFileSystem(rootBuilder(), CONTENT_ID, false);
		assertNotNull(read);
		assertThat(read.getFileName(), is("ignored.txt"));
		assertThat(read.getContentType(), is("text/plain"));
	}

	@Test
	public void writeExternalContentFileUsesDefaultNameWhenAttachmentHasContentTypeOnly() throws Exception {
		Path external = temp.getRoot().toPath().resolve("unsafe name.txt");
		Files.writeString(external, "external");
		AttachmentContent attachment = attachment(null, "text/plain");
		attachment.setExternalAbsoluteFilePath(external.toString());

		AbstractContentManager.writeExternalContentFile(rootBuilder(), attachment);

		AttachmentContent read = AbstractContentManager.getFromFileSystem(rootBuilder(), CONTENT_ID, false);
		assertNotNull(read);
		assertThat(read.getFileName(), is("content.txt"));
	}

	@Test
	public void getFromFileSystemReturnsNullForMissingFolderMetaOrContent() throws Exception {
		assertNull(AbstractContentManager.getFromFileSystem(rootBuilder(), CONTENT_ID, false));

		Files.createDirectories(balancedPath());
		assertNull(AbstractContentManager.getFromFileSystem(rootBuilder(), CONTENT_ID, false));

		AttachmentContent attachment = attachment("report.pdf", "application/pdf");
		AbstractContentManager.writeExternalContentFile(rootBuilder(), attachment);
		assertNull(AbstractContentManager.getFromFileSystem(rootBuilder(), CONTENT_ID, false));
	}

	@Test
	public void getFailsWhenImplementationClassCannotBeInstantiated() {
		Class<? extends AbstractContentManager> original = AbstractContentManager.IMPLEMENTATION_CLASS;
		AbstractContentManager.IMPLEMENTATION_CLASS = BadContentManager.class;
		try {
			IllegalArgumentException thrown = assertThrows(IllegalArgumentException.class, AbstractContentManager::get);
			assertThat(thrown.getMessage(), containsString(BadContentManager.class.getName()));
		}
		finally {
			AbstractContentManager.IMPLEMENTATION_CLASS = original;
		}
	}

	@Test
	public void canAccessContentAllowsSuperUserWithoutDelegatingPermissionChecks() throws Exception {
		bindUserToThread(new SuperUser());

		assertThat(AbstractContentManager.canAccessContent("customer", "module", "document", "group", "user", "biz", "attachment"), is(true));
	}

	@Test
	public void canAccessContentUsesBeanPermissionWhenAttributeNameIsNull() throws Exception {
		User user = mock(User.class);
		bindUserToThread(user);
		when(user.canReadBean("biz", "module", "document", "customer", "group", "user")).thenReturn(Boolean.TRUE);

		assertThat(AbstractContentManager.canAccessContent("customer", "module", "document", "group", "user", "biz", null), is(true));
	}

	@Test
	public void canAccessContentReturnsFalseWhenPermissionCheckCannotResolveMetadataOrBean() throws Exception {
		User user = mock(User.class);
		bindUserToThread(user);
		when(user.canAccessContent("biz", "module", "document", "customer", "group", "user", "attachment"))
				.thenThrow(new MetaDataException("missing document"))
				.thenThrow(new DomainException("missing bean"));

		assertThat(AbstractContentManager.canAccessContent("customer", "module", "document", "group", "user", "biz", "attachment"), is(false));
		assertThat(AbstractContentManager.canAccessContent("customer", "module", "document", "group", "user", "biz", "attachment"), is(false));
	}

	private AttachmentContent attachment(String fileName, String contentType) {
		AttachmentContent result = new AttachmentContent("customer", "module", "document", "group", "user", "biz", "attribute")
				.attachment(fileName, contentType, new byte[] { 1, 2, 3 })
				.markup("<svg />");
		result.setContentId(CONTENT_ID);
		result.setLastModified(new Date(1_700_000_000_000L));
		return result;
	}

	private StringBuilder rootBuilder() {
		return new StringBuilder(temp.getRoot().toString()).append(File.separator);
	}

	private Path balancedPath() {
		return temp.getRoot().toPath().resolve("12").resolve("34").resolve("56").resolve(CONTENT_ID.toLowerCase());
	}

	private static void bindUserToThread(User user) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getUser()).thenReturn(user);
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

	public static final class BadContentManager extends NoOpContentManager {
		private BadContentManager() {
			// not instantiable by AbstractContentManager.get()
		}
	}
}
