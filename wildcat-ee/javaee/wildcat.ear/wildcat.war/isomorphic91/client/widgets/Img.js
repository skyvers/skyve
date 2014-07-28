/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 





//>	@class	Img
//
//	The Img widget class implements a simple widget that displays a single image.
//
//  @treeLocation Client Reference/Foundation
//  @visibility external
//  @example img
//<

isc.defineClass("Img", "StatefulCanvas").addClassMethods({
    _buffer : [],
    urlForState : function (baseURL, selected, focused, state, pieceName, customState) {
        if (!baseURL) return baseURL;
        // short circuit to just return baseURL for the simple case
        if (!state && !pieceName && !selected && !focused && !customState) return baseURL;

        // break baseURL up into name and extension
        var period = baseURL.lastIndexOf(isc.dot),
            name = baseURL.substring(0, period),
            extension = baseURL.substring(period),
            buffer = this._buffer;

        buffer.length = 1;
        buffer[0] = name;
        // add selected
        if (selected) {
            buffer[1] = isc._underscore;
            buffer[2] = isc.StatefulCanvas.SELECTED;
        }
        if (focused) {
            buffer[3] = isc._underscore;
            buffer[4] = isc.StatefulCanvas.FOCUSED;
        }
        // add state
        if (state) {
            buffer[5] = isc._underscore;
            buffer[6] = state;
        }
        if (customState) {
            buffer[7] = isc._underscore;
            buffer[8] = customState;
        }
        // add pieceName
        if (pieceName) {
            buffer[9] = isc._underscore;
            buffer[10] = pieceName;
        }
        buffer[11] = extension;
        var result = buffer.join(isc._emptyString);
        return result;
    }
});

// add default properties
isc.Img.addProperties( {
    //> @attr	img.name	(string : "main" : IA)
	// The value of this attribute is specified as the value of the 'name' attribute in the
    // resulting HTML.
    // <p>
    // Note: this attribute is ignored if the imageType is set to "tile"
    // 
    // @visibility external
	//<
	name:"main",

    //>	@attr	img.src		(SCImgURL : "blank.gif" : [IRW])
    // The base filename for the image.
    // <P>
    // This value will be combined with any specified +link{statefulCanvas.state,state}
    // to form a combined URL, changing the appearance of the component as the
    // state changes.
    // <P>
    // The following table lists out the standard set of combined URLs that 
    // may be generated. Subclasses may support additional state-derived media of course.
    // Note that the src URL will be split such that the extension is always applied to the
    // end of the combined string. For example in the following table, if <code>src</code>
    // was set to <code>"blank.gif"</code>, the Selected+Focused URL would be 
    // <code>"blank_Selected_Focused.gif"</code>.
    // <table>
    // <tr><td><b>URL for Img source</b></td><td><b>Description</b></td></tr>
    // <tr><td><code><i>src</i>+<i>extension</i></code></td><td>Default URL</td></tr>
    // <tr><td><code><i>src</i>+"_Selected"+<i>extension</i></code></td>
    //      <td>Applied when +link{statefulCanvas.selected} is set to true</td></tr>
    // <tr><td><code><i>src</i>+"_Focused"+<i>extension</i></code></td>
    //      <td>Applied when the component has keyboard focus, if 
    //      +link{statefulCanvas.showFocused} is true, and 
    //      +link{statefulCanvas.showFocusedAsOver} is not true.</td></tr>
    // <tr><td><code><i>src</i>+"_Over"+<i>extension</i></code></td>
    //      <td>Applied when the user rolls over the component if
    //          +link{statefulCanvas.showRollOver} is set to true</td></tr>
    // <tr><td><code><i>src</i>+"_Down"+<i>extension</i></code></td>
    //      <td>Applied when the user presses the mouse button over over the component if
    //          +link{statefulCanvas.showDown} is set to true</td></tr>
    // <tr><td><code><i>src</i>+"_Disabled"+<i>extension</i></code></td>
    //      <td>Applied to +link{canvas.disabled} component
    //       if +link{statefulCanvas.showDisabled} is true.</td></tr>
    // <tr><td colspan=2><i>Combined states</i></td></tr>
    // <tr><td><code><i>src</i>+"_Selected_Focused"+<i>extension</i></code></td>
    //      <td>Combined Selected and focused state</td></tr>
    // <tr><td><code><i>src</i>+"_Selected_Over"+<i>extension</i></code></td>
    //      <td>Combined Selected and rollOver state</td></tr>
    // <tr><td><code><i>src</i>+"_Focused_Over"+<i>extension</i></code></td>
    //      <td>Combined Focused and rollOver state</td></tr>
    // <tr><td><code><i>src</i>+"_Selected_Focused_Over"+<i>extension</i></code></td>
    //      <td>Combined Selected, Focused and rollOver state</td></tr>
    // <tr><td><code><i>src</i>+"_Selected_Down"+<i>extension</i></code></td>
    //      <td>Combined Selected and mouse-down state</td></tr>
    // <tr><td><code><i>src</i>+"_Focused_Down"+<i>extension</i></code></td>
    //      <td>Combined Focused and mouse-down state</td></tr>
    // <tr><td><code><i>src</i>+"_Selected_Focused_Down"+<i>extension</i></code></td>
    //      <td>Combined Selected, Focused and mouse-down state</td></tr>
    // <tr><td><code><i>src</i>+"_Selected_Disabled"+<i>extension</i></code></td>
    //      <td>Combined Selected and Disabled state</td></tr>
    // </table>
    //
    // @group  appearance
    // @visibility external
    //<
	src:"blank.gif",
	
	//> @attr img.altText (String : null : IRW)
	// If specified this property will be included as the <code>alt</code> text for the image HMTL
	// element. This is useful for improving application accessibility.
	// <P>
	// <b><code>altText</code> and hover prompt / tooltip behavior:</b> Note that some
	// browsers, including Internet Explorer 9, show a native hover tooltip containing the 
	// img tag's <code>alt</code> attribute. Developers should not rely on this behavior to show
	// the user a hover prompt - instead the +link{img.prompt} attribute should be used.<br>
	// To set alt text <i>and</i> ensure a hover prompt shows up in all browsers, developers may
	// set +link{img.prompt} and <code>altText</code> to the same value. If both 
	// these attributes are set, the standard SmartClient prompt behavior will show a hover
	// prompt in most browsers, but will be suppressed for browsers where a native tooltip 
	// is shown for altText. Note that setting <code>altText</code> and <code>prompt</code> to
	// different values is not recommended - the prompt value will be ignored in favor of the
	// altText in this case.
	// @visibility external
	// @group accessibility
	//<
	
	//> @attr img.prompt (String : null : IRW)
	// @include canvas.prompt
	//<
	
   
    //>	@attr	img.activeAreaHTML		(String of HTML AREA tags : null : IRWA)
    //
    // Setting this attribute configures an image map for this image.  The value is expected as a
    // sequence of &lg;AREA&gt tags - e.g:
    // <pre>
    // Img.create({ 
    //     src: "myChart.gif",
    //     activeAreaHTML:
    //         "&lt;AREA shape='rect' coords='10,50,30,200' title='30' href='javascript:alert(\"30 units\")'&gt;" +
    //         "&lt;AREA shape='rect' coords='50,90,80,200' title='22' href='javascript:alert(\"22 units\")'&gt;"
    // });
    // </pre>
    // <u>Implementation notes:</u>
    // <ul>
    // <li>Quotes in the activeAreaHTML must be escaped or alternated appropriately.</li>
    // <li>Image maps do not stretch to fit scaled images. You must ensure that the dimensions of
    // your Img component match the anticipated width and height of your image map (which will typically
    // match the native dimensions of your image). </li>
    // <li>To change the image map of an existing Img component, first set yourImg.activeAreaHTML,
    // then call yourImg.markForRedraw(). Calls to yourImg.setSrc() will not automatically update the
    // image map. </li>
    // <li>activeAreaHTML is not supported on tiled Img components (imageType:"tile").</li> 
    // <li>Native browser support for image map focus/blur, keyboard events, and certain AREA tag
    // attributes (eg NOHREF, DEFAULT...) varies by platform. If your image map HTML uses attributes
    // beyond the basics (shape, coords, href, title), you should test on all supported browsers to
    // ensure that it functions as expected.</li>
    // </ul>
    // 
    // @group  appearance
    // @visibility external
    //<
 
    //>	@attr	img.imageType		(ImageStyle : isc.Img.STRETCH : [IRW])
    //          Indicates whether the image should be tiled/cropped, stretched, or centered when the
    //          size of this widget does not match the size of the image. 
    //          CENTER shows the image in it's natural size, but can't do so while the 
    //          transparency fix is active for IE. The transparency fix can be manually disabled
    //          by setting +link{usePNGFix} to false.
    //          See ImageStyle for further details.
    //      @visibility external
    //      @group  appearance
    //<
	imageType: isc.Img.STRETCH,

    //> @attr img.imageHeight (integer : null : IR)
    // Explicit size for the image, for +link{imageType} settings that would normally use the
    // image's natural size (applies to +link{img.imageType} "center" and "normal" only).
    // @visibility external
    //<

    //> @attr img.imageWidth (integer : null : IR)
    // Explicit size for the image, for +link{imageType} settings that would normally use the
    // image's natural size (applies to +link{img.imageType} "center" and "normal" only).
    // @visibility external
    //<

    //> @attr   img.size            (Number : null : [IR])
    // Convenience for setting height and width to the same value, at init time only
    // @group sizing
    // @visibility external
    //<

    // do set styling on the widget's handle
    suppressClassName:false,
    
    
    mozOutlineOffset:"0px",
    
    //> @attr img.showTitle (Boolean : false : [IRWA])
    // @include StatefulCanvas.showTitle
    // @visibility external
    //<
    showTitle:false,
    
    //> @attr img.usePNGFix (Boolean : true : [IR])
    // If false, never apply the png fix needed in Internet Explorer to make png transparency
    // work correctly.
    // @visibility external
    //<
    usePNGFix: true
});

// add methods to the class
isc.Img.addMethods({

initWidget : function () {
    // HACK: call Super the direct way   
    isc.StatefulCanvas._instancePrototype.initWidget.call(this);
    //this.Super(this._$initWidget);

    this.redrawOnResize = (this.imageType != isc.Img.STRETCH);
},

//> @method img.setImageType()
// Change the style of image rendering.
//
// @param imageType (ImageStyle) new style of image rendering
//
// @visibility external
//<
setImageType : function (imageType) {
    if (this.imageType == imageType) return;
    this.imageType = imageType;
    this.markForRedraw();
    this.redrawOnResize = (this.imageType != isc.Img.STRETCH);
},

getURL : function () {
    return this.statelessImage ? this.src : this.Super("getURL", arguments);
},

//>	@method	img.getInnerHTML()	(A)
//		@group	drawing
//			write the actual image for the contents
//
//		@return	(HTML)	HTML output for this canvas
//<

_$tableStart : "<TABLE WIDTH=",
_$heightEquals : " HEIGHT=",
_$tableTagClose : " BORDER=0 CELLSPACING=0 CELLPADDING=0><TR>",
_$centerCell : "<TD style='line-height:1px' VALIGN=center ALIGN=center>",
_$tileCell : "<TD BACKGROUND=",
_$tableEnd : "</TD></TR></TABLE>",
getInnerHTML : function () {
    var width = this.sizeImageToFitOverflow ? this.getOverflowedInnerWidth() 
                                            : this.getInnerWidth(),
        height = this.sizeImageToFitOverflow ? this.getOverflowedInnerHeight() 
                                            : this.getInnerHeight(),
        imageType = this.imageType;

    var extraStuff = this.extraStuff;
    if (this.imageStyle != null) {
        var classText = " class='" + this.imageStyle + this.getStateSuffix() + this._$singleQuote;
        if (extraStuff == null) extraStuff = classText;
        else extraStuff += classText;
    }
    if (this.altText != null) {
        var altText = this.altText;
        altText = " alt='" + altText.replace("'", "&apos;") + this._$singleQuote;
        if (extraStuff == null) extraStuff = altText;
        else extraStuff += altText;
    }

    // stretch: just use an <IMG> tag [default]
    if (imageType == isc.Img.STRETCH || imageType == isc.Img.NORMAL) {
        // normal: use an img, but don't size to the Canvas extents.  Size to imageWidth/Height
        // instead, which default to null.
        if (imageType == isc.Img.NORMAL) {
            width = this.imageWidth;
            height = this.imageHeight;
        }

        return this.imgHTML(this.getURL(), width, height, this.name, 
                            extraStuff, null, this.activeAreaHTML);
    }

    var output = isc.SB.create();
    // start padless/spaceless table
    output.append(this._$tableStart, width,
				        this._$heightEquals, height, this._$tableTagClose);

	if (imageType == isc.Img.TILE) {
        // tile: set image as background of a cell filled with a spacer
        
		output.append(this._$tileCell, this.getImgURL(this.getURL()), this._$rightAngle,
				      isc.Canvas.spacerHTML(width, height));
	} else { // (this.imageType == isc.Img.CENTER) 
        // center: place unsized image tag in center of cell
        
        output.append(this._$centerCell,
    				  this.imgHTML(this.getURL(), this.imageWidth, this.imageHeight, this.name, 
                                   extraStuff, null, this.activeAreaHTML));
	}

    output.append(this._$tableEnd);
    return output.toString();
},

// SizeToFitOverflow:
// If we're imageType:"stretch", and we're showing a label, the label contents may
// introduce overflow.
// This property can be set to cause our image to expand to fit under the overflowed label
sizeImageToFitOverflow:false,
getOverflowedInnerWidth : function () {
    return this.getVisibleWidth() - this.getHMarginBorder()
},

getOverflowedInnerHeight : function () {
    return this.getVisibleHeight() - this.getVMarginBorder()
},


_handleResized : function (deltaX, deltaY) {
    if (this.redrawOnResize != false || !this.isDrawn()) return;
   
    // if we're a stretch image, we can resize the image and not redraw it
    // TODO: in fact, we can reflow automatically in the same circumstances as the Button if we
    // draw similar HTML
    var imageStyle = this.getImage(this.name).style;
    var width = this.sizeImageToFitOverflow ? this.getOverflowedInnerWidth() :
                this.getInnerWidth(),
        height = this.sizeImageToFitOverflow ? this.getOverflowedInnerHeight() :
                this.getInnerHeight();
    
    this._assignSize(imageStyle, this._$width, width);
    this._assignSize(imageStyle, this._$height, height);
},
// 
_labelAdjustOverflow : function () {
    this.Super("_labelAdjustOverflow", arguments);
    if (this.overflow != isc.Canvas.VISIBLE || !this.sizeImageToFitOverflow) return;

    var image = this.getImage(this.name),
        imageStyle = image ? image.style : null;
    if (imageStyle == null) return;
    var width = this.getOverflowedInnerWidth(),
        height = this.getOverflowedInnerHeight();
        
    this._assignSize(imageStyle, this._$width, width);
    this._assignSize(imageStyle, this._$height, height);

},

//>	@method	img.setSrc()    ([])
// Changes the URL of this image and redraws it.
// <P>
// Does nothing if the src has not changed - if <code>src</code> has not changed but other
// state has changed such that the image needs updating, call +link{resetSrc()} instead.
//
// @param	URL		(URL)	new URL for the image
// @group	appearance
// @visibility external
// @example loadImages
//<
setSrc : function (URL) {
    if (URL == null || this.src == URL) return;

	this.src = URL;
    this.resetSrc();
},

//> @method img.resetSrc()   (A)
// Refresh the image being shown.  Call this when the +link{src} attribute has not changed, but
// other state that affects the image URL (such as being selected) has changed.
//
// @group	appearance
// @visibility external
//<
resetSrc : function () {
	if (!this.isDrawn()) return;

	// depending on how the image was originally drawn,
	//	we may be able to simply reset the image
	if (this.imageType != isc.Img.TILE) {
		this.setImage(this.name, this.getURL());
	// and we may have to redraw the whole thing
	} else {
		this.markForRedraw("setSrc on tiled image");
	}
},



//> @method img.stateChanged() 	 
//		Update the visible state of this image by changing the URL
//
//		@param  newState	(string)	name for the new state	 
//<
stateChanged : function () { 	 
	this.Super("stateChanged");
	
	// call resetSrc() with null to efficiently reset the image 	 
	if (!this.statelessImage) this.resetSrc(); 	 
},

//> @method img.getHoverHTML()
// If <code>this.showHover</code> is true, when the user holds the mouse over this Canvas for
// long enough to trigger a hover event, a hover canvas is shown by default. This method returns
// the contents of that hover canvas.
// <P>
// Overridden from Canvas: <br>
// If +link{prompt} is specified, and +link{altText} is unset, default implementation is unchanged -
// the prompt text will be displayed in the hover.<br>
// If +link{altText} and +link{prompt} are set this method will return null to suppress
// the standard hover behavior in browsers where the alt attribute on an img tag causes
// a native tooltip to appear, such as Internet Explorer.
// On other browsers the altText value will be returned.
//
//  @group hovers
//  @see canvas.showHover
//  @return (String) the string to show in the hover
//  @visibility external
//<
getHoverHTML : function () {
    if (this.altText) {
        
        if (isc.Browser.isIE) return null;
        // default to altText, not prompt so it's consistent cross-browser.
        if (this.prompt && this.prompt != this.altText) {
            this.logWarn("Img component specified with altText:" + this.altText 
                + " and prompt:" + this.prompt 
                + ". Value for 'prompt' attribute will be ignored in favor of 'altText' value.");
        }
        return this.altText
    }
    return this.Super("getHoverHTML", arguments);
}

});
