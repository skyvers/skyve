package modules.admin.HeapDumpList.actions;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.domain.HeapDumpList;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
public class DownloadHeapDumpTest extends AbstractH2Test {

	@Test
	void prepareWithNonExistentFileThrowsValidationException() {
		DownloadHeapDump action = new DownloadHeapDump();
		HeapDumpList bean = new HeapDumpList();
		bean.setSelectedName("nonexistent_heap_dump_that_does_not_exist.hprof");
		// file doesn't exist → throws ValidationException
		assertThrows(ValidationException.class, () -> action.prepare(bean, null));
	}
}
