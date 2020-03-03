{% if medium.mime %}
    <p>
        {{ medium.mime }} 
        {% if medium.filename %}
            {% if medium.width and medium.height %}
            &ndash; {{ medium.width }} x {{ medium.height }} {_ pixels _}
            {% endif %}
            {% if medium.size %}
                &ndash; {{ medium.size|filesizeformat }}
            {% endif %}
            &ndash; {{ medium.filename }}
        {% endif %}
        &ndash; {_ uploaded on _} {{ medium.created|date:"Y-m-d H:i:s" }}
    </p>

    <div class="edit-media {% if id.is_a.image %}do_cropcenter{% endif %}" id="rsc-image" data-original-width="{{ medium.width }}">
        {% if medium.width < 597 and medium.height < 597 %}
            {% media medium mediaclass="admin-media-cropcenter" %}
        {% else %}
            {% media medium mediaclass="admin-media" %}
        {% endif %}
    </div>

    <div class="save-buttons">
        {% if id.is_a.image %}
            <input type="hidden" name="crop_center" id="crop_center" value="{{ id.crop_center }}" />
            <a href="#" id="crop-center-remove" class="btn">
                <i class="icon-remove"></i> {_ Remove crop center _}
            </a>
            <span id="crop-center-message" class="muted">
                <span class="icon-info-sign"></span> {_ Click the image to set the cropping center. _}
            </span>
        {% endif %}

        <p class="text-right">
            {% if medium.size > 0 %}
                <a target="_blank" class="btn btn-default" href="{% url media_inline id=id %}" class="button">{_ View _}</a>
                <a target="_blank" class="btn btn-default" href="{% url media_attachment id=id %}" class="button">{_ Download _}</a>

                <input class="nosubmit" type="text" style="position: absolute; top:0; left:-9999px;" id="direct-download" value="{% url media_attachment id=id use_absolute_url %}">

                {% button
                    text=_"Copy download link"
                    class="btn btn-default"
                    action={script
                        script=["
                            var copyText = document.getElementById('direct-download');
                            copyText.select();
                            document.execCommand('copy');
                            z_growl_add('", _"Copied download link to clipboard" ,"');
                            return false;
                        "]
                    }
                %}
            {% endif %}

            {% button   text=_"Replace this media item"
                class="btn btn-primary"
                element="a"
                action={dialog_media_upload
                    id=id
                    action={update
                        target="media-edit-view"
                        template="_admin_edit_media_all.tpl"
                        id=id
                    }
                    center=0
                }
                disabled=not id.is_editable
            %}
        </p>

        {% if medium.is_av_sizelimit %}
            <p class="text-error text-right">
                <span class="icon-warning-sign"></span> {_ This file has not been scanned for viruses because it is too large. _}
            </p>
        {% elseif medium.is_av_scanned %}
            <p class="muted text-right">
                <span class="icon-ok-sign"></span> {_ This file has been scanned for viruses. _}
            </p>
        {% elseif m.modules.active.mod_clamav %}
            <p class="muted text-right">
                <span class="icon-info-sign"></span> {_ This file has not been scanned for viruses. _}
            </p>
        {% endif %}
    </div>
{% else %}
    {% if medium.created %}
        <p>
            {_ uploaded on _} {{ medium.created|date:"Y-m-d H:i:s" }}
        </p>
    {% endif %}
    <div class="save-buttons">
        <div class="pull-right">
            {% button   text=_"Replace this media item"
                class="btn btn-primary"
    	    action={dialog_media_upload id=id action={update target="media-edit-view" template="_admin_edit_media_all.tpl" id=id}} 
    	    disabled=not id.is_editable %}

        </div>
    </div>
{% endif %}
