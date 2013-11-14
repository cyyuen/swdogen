<?php

class InlineStyleSample {
    /**
     * @resource /inlineStyle "operation about inline style"
     * @basePath http://www.foo.com
     * @url /foo/foofoo
     * @apiName foofoo
     * @summary "this is a foo API"
     * @method POST
     * @notes "notes"
     * @param Bar:Bar #body "this is a bar"
     * @return BarBar(bar=Bar)
     *
     * @model Bar
     *
     * @property barbar:BarBar "barbarbar"
     *
     * @model BarBar
     *
     * @property bar:string "barbabrabrbarb ab"
     *
     */
    public function foofoo($Bar) {

    }
}
