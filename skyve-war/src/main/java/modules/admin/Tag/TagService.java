package modules.admin.Tag;

import java.util.ArrayList;
import java.util.List;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.skyve.tag.TagManager;

import jakarta.enterprise.inject.Default;

/**
 * This class acts as a service layer to encapsulate domain logic.
 *
 * Add this line to classes that wish to use it: @Inject private transient TagService tagService;
 */
@Default
public class TagService {
	/**
	 * Add to the subject tag records from the object tag.
	 * 
	 * @param subject
	 * @param object
	 */
	@SuppressWarnings("static-method")
	public void union(TagExtension subject, TagExtension object) throws Exception {
		if (subject != null && object != null) {
			// no dialect insensitive way to use SQL for creating new UUIDs for new records
			// add tagged items from object tag to subject tag
			// TagManager function deals with duplicates
			TagManager tm = EXT.getTagManager();
			try (AutoClosingIterable<Bean> i = tm.iterate(object.getBizId())) {
				for (Bean bean : i) {
					tm.tag(subject.getBizId(), bean);
				}
			}
			subject.setUploadTagged(
					Long.valueOf(subject.countDocument(subject.getUploadModuleName(), subject.getUploadDocumentName())));
			subject.setTotalTagged(Long.valueOf(subject.count()));
		}
	}

	/**
	 * Deletes from the subject Tag any items which are not also in the object
	 * Tag.
	 * 
	 * @param subject
	 * @param object
	 * @throws Exception
	 */
	@SuppressWarnings("static-method")
	public void intersect(TagExtension subject, TagExtension object) throws Exception {
		if (subject != null && object != null) {
			Persistence pers = CORE.getPersistence();

			// unsecured SQL for performance
			StringBuilder intersect = new StringBuilder();
			intersect.append("delete from ADM_Tagged ");
			intersect.append(" where taggedBizId not in (");
			intersect.append(" select taggedBizId from ADM_Tagged where ");
			intersect.append(" tag_id = :objectTagId");
			intersect.append(" and bizCustomer=:objectBizCustomer").append(" ) ");
			intersect.append(" and tag_id =:subjectTagId");
			intersect.append(" and bizCustomer=:subjectBizCustomer");

			SQL sql = pers.newSQL(intersect.toString());
			sql.putParameter("objectTagId", object.getBizId(), false);
			sql.putParameter("subjectTagId", subject.getBizId(), false);
			sql.putParameter("objectBizCustomer", subject.getBizCustomer(), false);
			sql.putParameter("subjectBizCustomer", subject.getBizCustomer(), false);

			sql.execute();

			subject.setUploadTagged(
					Long.valueOf(subject.countDocument(subject.getUploadModuleName(), subject.getUploadDocumentName())));
			subject.setTotalTagged(Long.valueOf(subject.count()));
		}
	}

	/**
	 * Deletes from the subject Tag items which are in the object Tag.
	 * 
	 * @param subject
	 * @param object
	 * @throws Exception
	 */
	@SuppressWarnings("static-method")
	public void except(TagExtension subject, TagExtension object) throws Exception {
		if (subject != null && object != null) {
			TagManager tm = EXT.getTagManager();
			try (AutoClosingIterable<Bean> i = tm.iterate(object.getBizId())) {
				for (Bean bean : i) {
					// TagManager method handles if this bean was not tagged
					tm.untag(subject.getBizId(), bean);
				}
			}
			subject.setUploadTagged(
					Long.valueOf(subject.countDocument(subject.getUploadModuleName(), subject.getUploadDocumentName())));
			subject.setTotalTagged(Long.valueOf(subject.count()));
		}
	}

	/**
	 * Retrieve the items tagged which match the specified module and document
	 * 
	 * @param mailout
	 * @return
	 * @throws Exception
	 */
	@SuppressWarnings("static-method")
	public List<Bean> getTaggedItemsForDocument(TagExtension tag, String moduleName, String documentName) throws Exception {

		List<Bean> beans = new ArrayList<>();
		if (moduleName != null && documentName != null) {
			Persistence pers = CORE.getPersistence();
			User user = pers.getUser();
			Customer customer = user.getCustomer();
			Module module = customer.getModule(moduleName);
			Document document = module.getDocument(customer, documentName);

			if (tag != null) {
				try (AutoClosingIterable<Bean> i = EXT.getTagManager().iterate(tag.getBizId())) {
					for (Bean bean : i) {
						if (bean != null && bean.getBizModule().equals(module.getName())
								&& bean.getBizDocument().equals(document.getName())) {
							// need to check that this is only done for documents of the selected type
							beans.add(bean);
						}
					}
				}
			}
		}

		return beans;
	}
}
