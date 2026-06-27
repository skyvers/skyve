package modules.kitchensink.domain;

import jakarta.annotation.Generated;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import java.util.List;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.domain.AbstractTransientBean;
import org.skyve.impl.domain.ChangeTrackingArrayList;

/**
 * Upload Fixture
 * <br/>
 * Upload fixture document
 * <br/>
 * Non-persistent Kitchen Sink fixture for manually inspecting upload.xhtml routes.
 * 
 * @navcomposed 1 gridRows 0..n UploadFixtureGridRow
 * @stereotype "transient"
 */
@XmlType
@XmlRootElement
@Generated(value = "org.skyve.impl.generate.OverridableDomainGenerator")
public class UploadFixture extends AbstractTransientBean {
	/**
	 * For Serialization
	 * @hidden
	 */
	private static final long serialVersionUID = 1L;

	/** @hidden */
	public static final String MODULE_NAME = "kitchensink";

	/** @hidden */
	public static final String DOCUMENT_NAME = "UploadFixture";

	/** @hidden */
	public static final String contentAutoNonePropertyName = "contentAutoNone";

	/** @hidden */
	public static final String contentAutoCameraPropertyName = "contentAutoCamera";

	/** @hidden */
	public static final String contentAutoVideoPropertyName = "contentAutoVideo";

	/** @hidden */
	public static final String contentAutoAllPropertyName = "contentAutoAll";

	/** @hidden */
	public static final String contentLinkNonePropertyName = "contentLinkNone";

	/** @hidden */
	public static final String contentLinkCameraPropertyName = "contentLinkCamera";

	/** @hidden */
	public static final String contentLinkVideoPropertyName = "contentLinkVideo";

	/** @hidden */
	public static final String contentLinkAllPropertyName = "contentLinkAll";

	/** @hidden */
	public static final String contentImageNonePropertyName = "contentImageNone";

	/** @hidden */
	public static final String contentImageCameraPropertyName = "contentImageCamera";

	/** @hidden */
	public static final String contentImageAllPropertyName = "contentImageAll";

	/** @hidden */
	public static final String contentVideoNonePropertyName = "contentVideoNone";

	/** @hidden */
	public static final String contentVideoVideoPropertyName = "contentVideoVideo";

	/** @hidden */
	public static final String contentVideoAllPropertyName = "contentVideoAll";

	/** @hidden */
	public static final String contentDisabledAutoPropertyName = "contentDisabledAuto";

	/** @hidden */
	public static final String contentDisabledImagePropertyName = "contentDisabledImage";

	/** @hidden */
	public static final String contentReadOnlyLinkPropertyName = "contentReadOnlyLink";

	/** @hidden */
	public static final String contentReadOnlyVideoPropertyName = "contentReadOnlyVideo";

	/** @hidden */
	public static final String gridRowsPropertyName = "gridRows";

	/**
	 * Auto / none
	 **/
	private String contentAutoNone;

	/**
	 * Auto / camera
	 **/
	private String contentAutoCamera;

	/**
	 * Auto / video
	 **/
	private String contentAutoVideo;

	/**
	 * Auto / all
	 **/
	private String contentAutoAll;

	/**
	 * Link / none
	 **/
	private String contentLinkNone;

	/**
	 * Link / camera
	 **/
	private String contentLinkCamera;

	/**
	 * Link / video
	 **/
	private String contentLinkVideo;

	/**
	 * Link / all
	 **/
	private String contentLinkAll;

	/**
	 * Image / none
	 **/
	private String contentImageNone;

	/**
	 * Image / camera
	 **/
	private String contentImageCamera;

	/**
	 * Image / all
	 **/
	private String contentImageAll;

	/**
	 * Video / none
	 **/
	private String contentVideoNone;

	/**
	 * Video / video
	 **/
	private String contentVideoVideo;

	/**
	 * Video / all
	 **/
	private String contentVideoAll;

	/**
	 * Disabled Auto Content
	 **/
	private String contentDisabledAuto;

	/**
	 * Disabled Image Content
	 **/
	private String contentDisabledImage;

	/**
	 * Read-only Link Content
	 **/
	private String contentReadOnlyLink;

	/**
	 * Read-only Video Content
	 **/
	private String contentReadOnlyVideo;

	/**
	 * Grid Rows
	 **/
	private List<UploadFixtureGridRow> gridRows = new ChangeTrackingArrayList<>("gridRows", this);

	@Override
	@XmlTransient
	public String getBizModule() {
		return UploadFixture.MODULE_NAME;
	}

	@Override
	@XmlTransient
	public String getBizDocument() {
		return UploadFixture.DOCUMENT_NAME;
	}

	public static UploadFixture newInstance() {
		try {
			return CORE.getUser().getCustomer().getModule(MODULE_NAME).getDocument(CORE.getUser().getCustomer(), DOCUMENT_NAME).newInstance(CORE.getUser());
		}
		catch (RuntimeException e) {
			throw e;
		}
		catch (Exception e) {
			throw new DomainException(e);
		}
	}

	@Override
	@XmlTransient
	public String getBizKey() {
		try {
			return org.skyve.util.Binder.formatMessage("Upload Fixture", this);
		}
		catch (@SuppressWarnings("unused") Exception e) {
			return "Unknown";
		}
	}

	/**
	 * {@link #contentAutoNone} accessor.
	 * @return	The value.
	 **/
	public String getContentAutoNone() {
		return contentAutoNone;
	}

	/**
	 * {@link #contentAutoNone} mutator.
	 * @param contentAutoNone	The new value.
	 **/
	@XmlElement
	public void setContentAutoNone(String contentAutoNone) {
		preset(contentAutoNonePropertyName, contentAutoNone);
		this.contentAutoNone = contentAutoNone;
	}

	/**
	 * {@link #contentAutoCamera} accessor.
	 * @return	The value.
	 **/
	public String getContentAutoCamera() {
		return contentAutoCamera;
	}

	/**
	 * {@link #contentAutoCamera} mutator.
	 * @param contentAutoCamera	The new value.
	 **/
	@XmlElement
	public void setContentAutoCamera(String contentAutoCamera) {
		preset(contentAutoCameraPropertyName, contentAutoCamera);
		this.contentAutoCamera = contentAutoCamera;
	}

	/**
	 * {@link #contentAutoVideo} accessor.
	 * @return	The value.
	 **/
	public String getContentAutoVideo() {
		return contentAutoVideo;
	}

	/**
	 * {@link #contentAutoVideo} mutator.
	 * @param contentAutoVideo	The new value.
	 **/
	@XmlElement
	public void setContentAutoVideo(String contentAutoVideo) {
		preset(contentAutoVideoPropertyName, contentAutoVideo);
		this.contentAutoVideo = contentAutoVideo;
	}

	/**
	 * {@link #contentAutoAll} accessor.
	 * @return	The value.
	 **/
	public String getContentAutoAll() {
		return contentAutoAll;
	}

	/**
	 * {@link #contentAutoAll} mutator.
	 * @param contentAutoAll	The new value.
	 **/
	@XmlElement
	public void setContentAutoAll(String contentAutoAll) {
		preset(contentAutoAllPropertyName, contentAutoAll);
		this.contentAutoAll = contentAutoAll;
	}

	/**
	 * {@link #contentLinkNone} accessor.
	 * @return	The value.
	 **/
	public String getContentLinkNone() {
		return contentLinkNone;
	}

	/**
	 * {@link #contentLinkNone} mutator.
	 * @param contentLinkNone	The new value.
	 **/
	@XmlElement
	public void setContentLinkNone(String contentLinkNone) {
		preset(contentLinkNonePropertyName, contentLinkNone);
		this.contentLinkNone = contentLinkNone;
	}

	/**
	 * {@link #contentLinkCamera} accessor.
	 * @return	The value.
	 **/
	public String getContentLinkCamera() {
		return contentLinkCamera;
	}

	/**
	 * {@link #contentLinkCamera} mutator.
	 * @param contentLinkCamera	The new value.
	 **/
	@XmlElement
	public void setContentLinkCamera(String contentLinkCamera) {
		preset(contentLinkCameraPropertyName, contentLinkCamera);
		this.contentLinkCamera = contentLinkCamera;
	}

	/**
	 * {@link #contentLinkVideo} accessor.
	 * @return	The value.
	 **/
	public String getContentLinkVideo() {
		return contentLinkVideo;
	}

	/**
	 * {@link #contentLinkVideo} mutator.
	 * @param contentLinkVideo	The new value.
	 **/
	@XmlElement
	public void setContentLinkVideo(String contentLinkVideo) {
		preset(contentLinkVideoPropertyName, contentLinkVideo);
		this.contentLinkVideo = contentLinkVideo;
	}

	/**
	 * {@link #contentLinkAll} accessor.
	 * @return	The value.
	 **/
	public String getContentLinkAll() {
		return contentLinkAll;
	}

	/**
	 * {@link #contentLinkAll} mutator.
	 * @param contentLinkAll	The new value.
	 **/
	@XmlElement
	public void setContentLinkAll(String contentLinkAll) {
		preset(contentLinkAllPropertyName, contentLinkAll);
		this.contentLinkAll = contentLinkAll;
	}

	/**
	 * {@link #contentImageNone} accessor.
	 * @return	The value.
	 **/
	public String getContentImageNone() {
		return contentImageNone;
	}

	/**
	 * {@link #contentImageNone} mutator.
	 * @param contentImageNone	The new value.
	 **/
	@XmlElement
	public void setContentImageNone(String contentImageNone) {
		preset(contentImageNonePropertyName, contentImageNone);
		this.contentImageNone = contentImageNone;
	}

	/**
	 * {@link #contentImageCamera} accessor.
	 * @return	The value.
	 **/
	public String getContentImageCamera() {
		return contentImageCamera;
	}

	/**
	 * {@link #contentImageCamera} mutator.
	 * @param contentImageCamera	The new value.
	 **/
	@XmlElement
	public void setContentImageCamera(String contentImageCamera) {
		preset(contentImageCameraPropertyName, contentImageCamera);
		this.contentImageCamera = contentImageCamera;
	}

	/**
	 * {@link #contentImageAll} accessor.
	 * @return	The value.
	 **/
	public String getContentImageAll() {
		return contentImageAll;
	}

	/**
	 * {@link #contentImageAll} mutator.
	 * @param contentImageAll	The new value.
	 **/
	@XmlElement
	public void setContentImageAll(String contentImageAll) {
		preset(contentImageAllPropertyName, contentImageAll);
		this.contentImageAll = contentImageAll;
	}

	/**
	 * {@link #contentVideoNone} accessor.
	 * @return	The value.
	 **/
	public String getContentVideoNone() {
		return contentVideoNone;
	}

	/**
	 * {@link #contentVideoNone} mutator.
	 * @param contentVideoNone	The new value.
	 **/
	@XmlElement
	public void setContentVideoNone(String contentVideoNone) {
		preset(contentVideoNonePropertyName, contentVideoNone);
		this.contentVideoNone = contentVideoNone;
	}

	/**
	 * {@link #contentVideoVideo} accessor.
	 * @return	The value.
	 **/
	public String getContentVideoVideo() {
		return contentVideoVideo;
	}

	/**
	 * {@link #contentVideoVideo} mutator.
	 * @param contentVideoVideo	The new value.
	 **/
	@XmlElement
	public void setContentVideoVideo(String contentVideoVideo) {
		preset(contentVideoVideoPropertyName, contentVideoVideo);
		this.contentVideoVideo = contentVideoVideo;
	}

	/**
	 * {@link #contentVideoAll} accessor.
	 * @return	The value.
	 **/
	public String getContentVideoAll() {
		return contentVideoAll;
	}

	/**
	 * {@link #contentVideoAll} mutator.
	 * @param contentVideoAll	The new value.
	 **/
	@XmlElement
	public void setContentVideoAll(String contentVideoAll) {
		preset(contentVideoAllPropertyName, contentVideoAll);
		this.contentVideoAll = contentVideoAll;
	}

	/**
	 * {@link #contentDisabledAuto} accessor.
	 * @return	The value.
	 **/
	public String getContentDisabledAuto() {
		return contentDisabledAuto;
	}

	/**
	 * {@link #contentDisabledAuto} mutator.
	 * @param contentDisabledAuto	The new value.
	 **/
	@XmlElement
	public void setContentDisabledAuto(String contentDisabledAuto) {
		preset(contentDisabledAutoPropertyName, contentDisabledAuto);
		this.contentDisabledAuto = contentDisabledAuto;
	}

	/**
	 * {@link #contentDisabledImage} accessor.
	 * @return	The value.
	 **/
	public String getContentDisabledImage() {
		return contentDisabledImage;
	}

	/**
	 * {@link #contentDisabledImage} mutator.
	 * @param contentDisabledImage	The new value.
	 **/
	@XmlElement
	public void setContentDisabledImage(String contentDisabledImage) {
		preset(contentDisabledImagePropertyName, contentDisabledImage);
		this.contentDisabledImage = contentDisabledImage;
	}

	/**
	 * {@link #contentReadOnlyLink} accessor.
	 * @return	The value.
	 **/
	public String getContentReadOnlyLink() {
		return contentReadOnlyLink;
	}

	/**
	 * {@link #contentReadOnlyLink} mutator.
	 * @param contentReadOnlyLink	The new value.
	 **/
	@XmlElement
	public void setContentReadOnlyLink(String contentReadOnlyLink) {
		preset(contentReadOnlyLinkPropertyName, contentReadOnlyLink);
		this.contentReadOnlyLink = contentReadOnlyLink;
	}

	/**
	 * {@link #contentReadOnlyVideo} accessor.
	 * @return	The value.
	 **/
	public String getContentReadOnlyVideo() {
		return contentReadOnlyVideo;
	}

	/**
	 * {@link #contentReadOnlyVideo} mutator.
	 * @param contentReadOnlyVideo	The new value.
	 **/
	@XmlElement
	public void setContentReadOnlyVideo(String contentReadOnlyVideo) {
		preset(contentReadOnlyVideoPropertyName, contentReadOnlyVideo);
		this.contentReadOnlyVideo = contentReadOnlyVideo;
	}

	/**
	 * {@link #gridRows} accessor.
	 * @return	The value.
	 **/
	@XmlElement
	public List<UploadFixtureGridRow> getGridRows() {
		return gridRows;
	}

	/**
	 * {@link #gridRows} accessor.
	 * @param bizId	The bizId of the element in the list.
	 * @return	The value of the element in the list.
	 **/
	public UploadFixtureGridRow getGridRowsElementById(String bizId) {
		return getElementById(gridRows, bizId);
	}

	/**
	 * {@link #gridRows} mutator.
	 * @param bizId	The bizId of the element in the list.
	 * @param element	The new value of the element in the list.
	 **/
	public void setGridRowsElementById(String bizId, UploadFixtureGridRow element) {
		setElementById(gridRows, element);
	}

	/**
	 * {@link #gridRows} add.
	 * @param element	The element to add.
	 **/
	public boolean addGridRowsElement(UploadFixtureGridRow element) {
		boolean result = gridRows.add(element);
		if (result) {
			element.setParent(this);
		}
		return result;
	}

	/**
	 * {@link #gridRows} add.
	 * @param index	The index in the list to add the element to.
	 * @param element	The element to add.
	 **/
	public void addGridRowsElement(int index, UploadFixtureGridRow element) {
		gridRows.add(index, element);
		element.setParent(this);
	}

	/**
	 * {@link #gridRows} remove.
	 * @param element	The element to remove.
	 **/
	public boolean removeGridRowsElement(UploadFixtureGridRow element) {
		boolean result = gridRows.remove(element);
		if (result) {
			element.setParent(null);
		}
		return result;
	}

	/**
	 * {@link #gridRows} remove.
	 * @param index	The index in the list to remove the element from.
	 **/
	public UploadFixtureGridRow removeGridRowsElement(int index) {
		UploadFixtureGridRow result = gridRows.remove(index);
		result.setParent(null);
		return result;
	}
}
