package modules.admin.HeapDumpList.models;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.metadata.view.model.list.Page;

import modules.admin.domain.DownloadFolder;
import util.AbstractH2Test;

class HeapDumpsModelH2Test extends AbstractH2Test {
	@TempDir
	private Path tempDir;

	@Test
	@SuppressWarnings("static-method")
	void postConstructPopulatesDrivingDocumentColumnsAndProjections() {
		HeapDumpsModel model = new HeapDumpsModel();

		model.postConstruct(CORE.getCustomer(), false);

		assertThat(model.getDescription(), is("All Heap Dumps"));
		assertThat(model.getDrivingDocument().getName(), is(DownloadFolder.DOCUMENT_NAME));
		assertThat(model.getColumns().size(), is(1));
		assertThat(model.getColumns().get(0).getBinding(), is(DownloadFolder.namePropertyName));
		assertThat(model.getProjections().contains(Bean.DOCUMENT_ID), is(true));
		assertThat(model.getProjections().contains(DownloadFolder.namePropertyName), is(true));
		assertThat(model.getProjections().contains(DownloadFolder.sizePropertyName), is(true));
		assertNull(model.getFilter());
		assertNull(model.newFilter());
		model.putParameter("ignored", "ignored");
	}

	@Test
	void fetchHeapDumpsReturnsDescendingPagedHprofFilesOnly() throws Exception {
		Files.writeString(tempDir.resolve("alpha.hprof"), "alpha", StandardCharsets.UTF_8);
		Files.writeString(tempDir.resolve("omega.hprof"), "omega", StandardCharsets.UTF_8);
		Files.writeString(tempDir.resolve("ignored.txt"), "ignored", StandardCharsets.UTF_8);

		Page page = invokeFetchHeapDumps(tempDir.toString(), 0, 0);

		assertThat(page.getTotalRows(), is(2L));
		assertThat(page.getRows().size(), is(1));
		Bean row = page.getRows().get(0);
		assertThat(row, instanceOf(DynamicBean.class));
		DynamicBean dynamicRow = (DynamicBean) row;
		assertThat(dynamicRow.getBizModule(), is(DownloadFolder.MODULE_NAME));
		assertThat(dynamicRow.getBizDocument(), is(DownloadFolder.DOCUMENT_NAME));
		assertThat(dynamicRow.get(DownloadFolder.namePropertyName), is("omega.hprof"));
		assertThat(page.getSummary().getBizDocument(), is(DownloadFolder.DOCUMENT_NAME));
	}

	@Test
	void fetchHeapDumpsReturnsEmptyPageWhenNoHprofFilesExist() throws Exception {
		Files.writeString(tempDir.resolve("ignored.txt"), "ignored", StandardCharsets.UTF_8);

		Page page = invokeFetchHeapDumps(tempDir.toString(), 0, 10);

		assertThat(page.getTotalRows(), is(0L));
		assertThat(page.getRows().isEmpty(), is(true));
		assertThat(page.getSummary().getBizModule(), is(DownloadFolder.MODULE_NAME));
	}

	@Test
	@SuppressWarnings("static-method")
	void unsupportedListModelOperationsThrow() throws Exception {
		HeapDumpsModel model = new HeapDumpsModel();

		assertThrows(IllegalStateException.class, model::iterate);
		assertThrows(IllegalStateException.class, () -> model.update("id", new TreeMap<>()));
		assertThrows(IllegalStateException.class, () -> model.remove("id"));
	}

	private static Page invokeFetchHeapDumps(String dirPath, int startRow, int endRow) throws Exception {
		Method method = HeapDumpsModel.class.getDeclaredMethod("fetchHeapDumps", String.class, int.class, int.class);
		method.setAccessible(true);
		return (Page) method.invoke(null, dirPath, Integer.valueOf(startRow), Integer.valueOf(endRow));
	}
}
