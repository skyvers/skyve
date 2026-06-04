package modules.admin.Tag.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.UploadException;
import org.skyve.domain.messages.UploadException.Problem;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.WebFileInputStream;

import modules.admin.Tag.TagExtension;

@SuppressWarnings("static-method")
class UploadTagCriteriaTest {
	@Test
	void uploadWithUnsupportedExtensionAddsErrorAndThrowsUploadException() {
		UploadTagCriteria action = new UploadTagCriteria();
		TagExtension bean = new TagExtension();
		Upload upload = upload("criteria.csv");
		UploadException uploadException = new UploadException();
		UploadException thrown = assertThrows(UploadException.class,
				() -> action.upload(bean, upload, uploadException, null));

		Problem problem = thrown.getErrors().iterator().next();
		assertThat(problem.getWhat(), is("Only xlsx files are supported"));
	}

	@SuppressWarnings("resource")
	private static Upload upload(String fileName) {
		return new Upload(fileName,
				new WebFileInputStream(new ByteArrayInputStream("code".getBytes(StandardCharsets.UTF_8))),
				MimeType.csv);
	}
}
