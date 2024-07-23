package modules.admin.ControlPanel;

import java.util.Date;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;
import org.skyve.tag.TagManager;
import org.skyve.util.Binder;
import org.skyve.util.DataBuilder;
import org.skyve.util.PushMessage;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.ModuleDocument;
import modules.admin.domain.Tag;

public class GenerateTestDataJob extends CancellableJob {
	private DataBuilder db;
	
	@Override
	public void execute() throws Exception {
		EXT.push(new PushMessage().user(CORE.getUser()).growl(MessageSeverity.info,
				"Generate Test Data Job has been started"));
		List<String> log = getLog();
		Customer customer = CORE.getCustomer();
		Persistence pers = CORE.getPersistence();
		ControlPanelExtension bean = (ControlPanelExtension) getBean();
		Tag tag = createOrRetrieveTag(pers, bean);
		
		// create a DataBuilder which will also populate optional references
		db = new DataBuilder()
				.fixture(FixtureType.crud)
				.optional(true, true)
				.depth(10);

		int failed = 0;
		int size = bean.getTestDocumentNames().size() * bean.getTestNumberToGenerate().intValue();
		
		TagManager tm = EXT.getTagManager();
		for (ModuleDocument docName : bean.getTestDocumentNames()) {
			for (int i = 0; i < bean.getTestNumberToGenerate().intValue(); i++) {
				try {
					Module module = customer.getModule(docName.getModuleName());
					Document document = module.getDocument(customer, docName.getDocumentName());

					PersistentBean newTestDataItem = db.build(module, document);
					pers.save(document, newTestDataItem);
					if (Boolean.TRUE.equals(bean.getTestTagGeneratedData())) {
						tm.tag(tag.getBizId(), module.getName(), document.getName(), newTestDataItem.getBizId());
					}

					log.add(Binder.formatMessage("Succesfully created {moduleName}.{documentName} ", docName)
							+ newTestDataItem.getBizKey());

					pers.commit(false);
					pers.evictCached(newTestDataItem);
					pers.begin();

				} catch (Exception e) {
					failed++;
					log.add(String.format("Error creating instance of %s - %s", docName.getDocumentName(), e.getMessage()));
					e.printStackTrace();
				}
				setPercentComplete((int) (((float) i) / ((float) size) * 100F));
			}
			log.add("");
		}
	
		setPercentComplete(100);
		int successful = (bean.getTestNumberToGenerate().intValue() * bean.getTestDocumentNames().size()) - failed;
		log.add("Finished Generate Test Data job at " + new Date());
		log.add(successful + " Documents successfully created, " + failed + " failed.");
		EXT.push(new PushMessage().user(CORE.getUser()).growl(MessageSeverity.info,
				String.format("%d Documents successfully created", Integer.valueOf(successful))));
	}

	private static Tag createOrRetrieveTag(Persistence pers, ControlPanelExtension bean) {
		Tag tag = null;

		if (Boolean.TRUE.equals(bean.getTestTagGeneratedData())) {
			DocumentQuery q = pers.newDocumentQuery(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
			q.getFilter().addEquals(Tag.namePropertyName, bean.getTestTagName());
			tag = q.beanResult();

			// create a new tag if it does not exist
			if (tag == null) {
				tag = Tag.newInstance();
				tag.setName(bean.getTestTagName());
				tag.setVisible(Boolean.TRUE);
				tag = pers.save(tag);
			}
		}
		return tag;
	}
}
